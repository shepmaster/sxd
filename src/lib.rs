#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use async_trait::async_trait;
use easy_ext::ext;
use snafu::{ensure, Snafu};
use std::{future::Future, mem, str};

#[macro_use]
mod macros;

pub mod blocking;

#[async_trait(?Send)]
pub trait DataSource {
    async fn read(&mut self, buffer: &mut [u8]) -> usize;
}

#[derive(Debug)]
struct StringRing<S> {
    buffer: Vec<u8>,

    n_offset_bytes: usize,
    n_utf8_bytes: usize,
    n_dangling_bytes: usize,

    n_retired_bytes: usize,

    source: S,
}

impl<S> StringRing<S>
where
    S: DataSource,
{
    // The longest single token should be `standalone`, then round up
    // a bit.
    const MINIMUM_CAPACITY: usize = 16;
    const DEFAULT_CAPACITY: usize = 1024;

    fn new(source: S) -> Self {
        Self::with_buffer_capacity(source, Self::DEFAULT_CAPACITY)
    }

    fn with_buffer_capacity(source: S, capacity: usize) -> Self {
        assert!(
            capacity >= Self::MINIMUM_CAPACITY,
            "The capacity must be large enough to match minimum token lengths",
        );

        Self {
            buffer: vec![0; capacity],

            n_offset_bytes: 0,
            n_utf8_bytes: 0,
            n_dangling_bytes: 0,

            n_retired_bytes: 0,

            source,
        }
    }

    fn as_str(&self) -> &str {
        let bytes = &self.buffer[self.n_offset_bytes..][..self.n_utf8_bytes];

        // SAFETY: The range of bytes that are valid UTF-8 is checked
        // when `extend`ing the buffer and when advancing it. Checking
        // it again here leads to a massive (~1000X) slowdown.
        unsafe { str::from_utf8_unchecked(bytes) }
    }

    async fn min_str(&mut self, len: usize) -> Result<&str> {
        if self.n_utf8_bytes < len {
            self.extend().await?;
        }
        Ok(self.as_str())
    }

    async fn some_str(&mut self) -> Result<&str> {
        self.min_str(1).await
    }

    fn absolute_location(&self) -> usize {
        self.n_retired_bytes + self.n_offset_bytes
    }

    async fn extend(&mut self) -> Result<()> {
        let mut buffer =
            &mut self.buffer[self.n_offset_bytes..][self.n_utf8_bytes..][self.n_dangling_bytes..];

        if buffer.is_empty() {
            let s = self.n_offset_bytes;
            let e = s + self.n_utf8_bytes + self.n_dangling_bytes;
            self.buffer.copy_within(s..e, 0);
            self.n_offset_bytes = 0;
            self.n_retired_bytes += s;

            buffer = &mut self.buffer[self.n_offset_bytes..][self.n_utf8_bytes..]
                [self.n_dangling_bytes..];
        }
        assert!(
            !buffer.is_empty(),
            "No room left to store data in buffer; buffer too small",
        );

        let n_new_bytes = self.source.read(buffer).await;
        ensure!(n_new_bytes > 0, NoMoreInputAvailable);

        self.n_dangling_bytes += n_new_bytes;

        let dangling_bytes =
            &self.buffer[self.n_offset_bytes..][self.n_utf8_bytes..][..self.n_dangling_bytes];

        // SAFETY: This helps uphold the safety invariants in `as_str`
        let n_new_utf8_bytes = match str::from_utf8(dangling_bytes) {
            Ok(s) => s.len(),
            Err(e) => match e.error_len() {
                Some(length) => {
                    return InputNotUtf8 {
                        location: self.absolute_location() + self.n_utf8_bytes + e.valid_up_to(),
                        length,
                    }
                    .fail()
                }
                None => e.valid_up_to(),
            },
        };

        self.n_dangling_bytes -= n_new_utf8_bytes;
        self.n_utf8_bytes += n_new_utf8_bytes;

        Ok(())
    }

    async fn complete(&mut self) -> Result<bool> {
        if !self.as_str().is_empty() {
            return Ok(false);
        }

        match self.extend().await {
            Ok(()) => Ok(self.as_str().is_empty()),
            Err(Error::NoMoreInputAvailable) => Ok(true),
            Err(e) => Err(e),
        }
    }

    async fn starts_with(&mut self, needle: &str) -> Result<bool> {
        while self.n_utf8_bytes < needle.len() {
            // TODO: Avoid infinite loop
            match self.extend().await {
                Ok(_) => {}
                Err(Error::NoMoreInputAvailable) => return Ok(false),
                error => error?,
            }
        }

        Ok(self.as_str().starts_with(needle))
    }

    fn advance(&mut self, n_bytes: usize) {
        // SAFETY: These help uphold the safety invariants in `as_str`
        assert!(n_bytes <= self.n_utf8_bytes);
        assert!(self.as_str().is_char_boundary(n_bytes));

        self.n_offset_bytes += n_bytes;
        self.n_utf8_bytes -= n_bytes;
    }

    async fn consume(&mut self, s: impl AsRef<str>) -> Result<MustUse<bool>> {
        let s = s.as_ref();

        if self.starts_with(s).await? {
            self.advance(s.len());
            Ok(MustUse(true))
        } else {
            Ok(MustUse(false))
        }
    }

    async fn require_or_else(&mut self, s: &str, e: impl FnOnce(usize) -> Error) -> Result<()> {
        if *self.consume(s).await? {
            Ok(())
        } else {
            Err(e(self.absolute_location()))
        }
    }

    async fn require(&mut self, s: impl AsRef<str>) -> Result<()> {
        let token = s.as_ref();

        self.require_or_else(token, |location| {
            RequiredTokenMissing { token, location }.build()
        })
        .await
    }

    async fn require_quote(&mut self) -> Result<Quote> {
        if *self.consume(&Quote::Double).await? {
            Ok(Quote::Double)
        } else if *self.consume(&Quote::Single).await? {
            Ok(Quote::Single)
        } else {
            let location = self.absolute_location();
            ExpectedSingleOrDoubleQuote { location }.fail()
        }
    }

    /// Warning: This only works for single bytes!
    async fn consume_until(&mut self, needle: impl AsRef<str>) -> Result<Streaming<&str>> {
        let mut s = self.as_str();
        if s.is_empty() {
            self.extend().await?;
            s = self.as_str();
        }

        match s.find(needle.as_ref()) {
            Some(x) => Ok(Streaming::Complete(&self.as_str()[..x])),
            None => Ok(Streaming::Partial(self.as_str())),
        }
    }

    /// Anything that's not `<` or `&` so long as it doesn't include `]]>`
    async fn char_data(&mut self) -> Result<Option<usize>> {
        let s = self.some_str().await?;

        let offset = match s.find(&['<', '&'][..]) {
            Some(0) => return Ok(None),
            Some(offset) => offset,
            None => s.len(),
        };
        // TODO: This probably doesn't work at a buffer boundary
        let offset = s[..offset].find("]]>").unwrap_or(offset);

        Ok(Some(offset))
    }

    /// Anything that's not `]]>`
    async fn cdata(&mut self) -> Result<Streaming<&str>> {
        let s = self.min_str(3).await?;

        match s.find("]]>") {
            Some(offset) => return Ok(Streaming::Complete(&s[..offset])),
            None => {
                // Once for each `]`
                let s = s.strip_suffix(']').unwrap_or(s);
                let s = s.strip_suffix(']').unwrap_or(s);
                Ok(Streaming::Partial(s))
            }
        }
    }

    async fn processing_instruction_value(&mut self) -> Result<Streaming<&str>> {
        let s = self.min_str(2).await?;

        match s.find("?>") {
            Some(offset) => Ok(Streaming::Complete(&s[..offset])),
            None => {
                let s = s.strip_suffix('?').unwrap_or(s);
                Ok(Streaming::Partial(s))
            }
        }
    }

    async fn comment(&mut self) -> Result<Streaming<&str>> {
        let s = self.min_str(2).await?;

        match s.find("--") {
            Some(offset) => Ok(Streaming::Complete(&s[..offset])),
            None => {
                let s = s.strip_suffix('-').unwrap_or(s);
                Ok(Streaming::Partial(s))
            }
        }
    }

    async fn space(&mut self) -> Result<Option<usize>> {
        let s = self.some_str().await?;
        match matching_bytes(s, char::is_space) {
            0 => Ok(None),
            len => Ok(Some(len)),
        }
    }

    async fn consume_space(&mut self) -> Result<usize> {
        let mut total = 0;

        while let Some(len) = self.space().await? {
            total += len;
            self.advance(len);
        }

        Ok(total)
    }

    async fn reference_decimal(&mut self) -> Result<Streaming<&str>> {
        self.while_char(char::is_ascii_digit).await
    }

    async fn reference_hex(&mut self) -> Result<Streaming<&str>> {
        self.while_char(char::is_ascii_hexdigit).await
    }

    async fn name(&mut self) -> Result<Streaming<&str>> {
        let s = self.some_str().await?;

        let mut c = s.char_indices();

        let end_idx = match c
            .next()
            .filter(|(_, c)| c.is_name_start_char())
            .map(|(i, c)| i + c.len_utf8())
        {
            Some(i) => i,
            None => return Ok(Streaming::Complete("")),
        };

        let end_idx = c
            .take_while(|(_, c)| c.is_name_char())
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(end_idx);

        if end_idx == s.len() {
            Ok(Streaming::Partial(s))
        } else {
            Ok(Streaming::Complete(&s[..end_idx]))
        }
    }

    async fn name_continuation(&mut self) -> Result<Streaming<&str>> {
        self.while_char(char::is_name_char).await
    }

    #[inline]
    async fn while_char(&mut self, predicate: impl Fn(&char) -> bool) -> Result<Streaming<&str>> {
        let s = self.some_str().await?;

        match matching_bytes(s, predicate) {
            offset if offset == s.len() => Ok(Streaming::Partial(s)),
            offset => Ok(Streaming::Complete(&s[..offset])),
        }
    }
}

#[inline]
fn matching_bytes(s: &str, predicate: impl Fn(&char) -> bool) -> usize {
    s.char_indices()
        .take_while(|(_, c)| predicate(c))
        .last()
        .map(|(i, c)| i + c.len_utf8())
        .unwrap_or(0)
}

#[ext]
impl char {
    #[inline]
    fn is_space(&self) -> bool {
        match self {
            '\u{20}' | '\u{9}' | '\u{D}' | '\u{A}' => true,
            _ => false,
        }
    }

    #[inline]
    fn is_name_start_char_ascii(self) -> bool {
        match self {
            ':' | 'A'..='Z' | '_' | 'a'..='z' => true,
            _ => false,
        }
    }

    #[inline]
    fn is_name_start_char(self) -> bool {
        if self.is_name_start_char_ascii() {
            return true;
        }

        match self {
            '\u{C0}'..='\u{D6}'
            | '\u{D8}'..='\u{F6}'
            | '\u{F8}'..='\u{2FF}'
            | '\u{370}'..='\u{37D}'
            | '\u{37F}'..='\u{1FFF}'
            | '\u{200C}'..='\u{200D}'
            | '\u{2070}'..='\u{218F}'
            | '\u{2C00}'..='\u{2FEF}'
            | '\u{3001}'..='\u{D7FF}'
            | '\u{F900}'..='\u{FDCF}'
            | '\u{FDF0}'..='\u{FFFD}'
            | '\u{10000}'..='\u{EFFFF}' => true,
            _ => false,
        }
    }

    #[inline]
    fn is_name_char_ascii(&self) -> bool {
        if self.is_name_start_char_ascii() {
            return true;
        }

        match self {
            '-' | '.' | '0'..='9' => true,
            _ => false,
        }
    }

    #[inline]
    fn is_name_char(&self) -> bool {
        if self.is_name_char_ascii() {
            return true;
        }

        if self.is_name_start_char() {
            return true;
        }

        match self {
            '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}' => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    Initial,

    StreamDeclarationVersion(Quote),
    AfterDeclarationVersion,

    StreamElementOpenName,
    AfterElementOpenName,

    StreamAttributeName,
    AfterAttributeName,
    StreamAttributeValue(Quote),

    StreamElementCloseName,
    AfterElementCloseName,

    StreamReferenceNamed,
    StreamReferenceDecimal,
    StreamReferenceHex,
    AfterReference,

    StreamProcessingInstructionName,
    AfterProcessingInstructionName,
    StreamProcessingInstructionValue,
    AfterProcessingInstructionValue,

    StreamCData,
    AfterCData,

    StreamComment,
    AfterComment,
}

#[derive(Debug, Copy, Clone, Eq)]
pub enum Streaming<T> {
    Partial(T),
    Complete(T),
}

impl<T> Streaming<T> {
    fn is_complete(&self) -> bool {
        matches!(self, Streaming::Complete(_))
    }

    fn map<U>(self, f: impl FnOnce(T) -> U) -> Streaming<U> {
        use Streaming::*;

        match self {
            Partial(v) => Partial(f(v)),
            Complete(v) => Complete(f(v)),
        }
    }

    fn unify(&self) -> &T {
        use Streaming::*;

        match self {
            Partial(a) => a,
            Complete(a) => a,
        }
    }
}

impl<T, U> PartialEq<Streaming<U>> for Streaming<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Streaming<U>) -> bool {
        match (self, other) {
            (Streaming::Partial(a), Streaming::Partial(b)) => a == b,
            (Streaming::Complete(a), Streaming::Complete(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Quote {
    Single,
    Double,
}

impl Quote {
    fn to_char(&self) -> char {
        match self {
            Self::Single => '\'',
            Self::Double => '"',
        }
    }
}

impl AsRef<str> for Quote {
    fn as_ref(&self) -> &str {
        match self {
            Self::Single => "'",
            Self::Double => "\"",
        }
    }
}

macro_rules! impl_token {
    (
        $(#[$o_meta:meta])*
        $vis:vis enum $e_name:ident<$g:ident> {
            $(
                $(#[$i_meta:meta])*
                $v_name:ident $(($field:ident))?
            ,)*
        }
    ) => {
        $(#[$o_meta])*
        $vis enum $e_name<$g> {
            $($(#[$i_meta])* $v_name $(($field))?,)*
        }

        impl<$g> $e_name<$g> {
            #[allow(non_snake_case)]
            fn map<U>(self, f: impl FnOnce($g) -> U) ->  $e_name<U> {
                use $e_name::*;

                match self {
                    $($v_name $(($field))? => $v_name $((f($field)))?, )*
                }
            }
        }

        impl<$g, U> PartialEq<$e_name<U>> for $e_name<$g>
        where
            $g: PartialEq<U>,
        {
            fn eq(&self, other: &$e_name<U>) -> bool {
                use $e_name::*;

                match (self, other) {
                    $(impl_token!(@eq_pat s1 s2 $v_name $($field)?) =>
                      impl_token!(@eq_arm s1 s2 $v_name $($field)?) ,)*
                    _ => false,
                }
            }
        }
    };

    (@eq_pat $s1:ident $s2:ident $v_name:ident $field:ty) => { ($v_name($s1), $v_name($s2)) };
    (@eq_arm $s1:ident $s2:ident $v_name:ident $field:ty) => { $s1 == $s2 };
    (@eq_pat $s1:ident $s2:ident $v_name:ident) => { ($v_name, $v_name) };
    (@eq_arm $s1:ident $s2:ident $v_name:ident) => { true };
}

impl_token! {
    #[derive(Debug, Copy, Clone, Eq)]
    pub enum Token<T> {
        /// `<?xml version="1.9"`
        DeclarationStart(T),
        /// `?>`
        DeclarationClose,

        /// `<foo`
        ElementOpenStart(T),
        /// `>`
        ElementOpenEnd,
        /// `/>`
        ElementSelfClose,

        /// `</foo`
        ElementClose(T),

        /// `foo`
        AttributeName(T),
        /// `="bar`
        AttributeValue(T),

        /// `hello world`
        CharData(T),
        /// `<![CDATA[hello world]]>`
        CData(T),
        Space(T),

        /// &lt;
        ReferenceNamed(T),
        /// &#4242;
        ReferenceDecimal(T),
        /// &#xABCD;
        ReferenceHex(T),

        /// `<?a`
        ProcessingInstructionStart(T),
        /// `b`
        ProcessingInstructionValue(T),
        /// `?>`
        ProcessingInstructionEnd,

        /// `<!--a-->`
        Comment(T),
    }
}

type TokenS<'a> = Token<Streaming<&'a str>>;

#[derive(Debug)]
pub struct Parser<S> {
    buffer: StringRing<S>,
    state: State,
    to_advance: usize,
}

impl<S> Parser<S>
where
    S: DataSource,
{
    pub fn new(source: S) -> Self {
        Self::with_buffer_capacity(source, StringRing::<S>::DEFAULT_CAPACITY)
    }

    pub fn with_buffer_capacity(source: S, capacity: usize) -> Self {
        Parser {
            buffer: StringRing::with_buffer_capacity(source, capacity),
            state: State::Initial,
            to_advance: 0,
        }
    }

    pub async fn next(&mut self) -> Option<Result<TokenS<'_>>> {
        use State::*;

        let to_advance = mem::take(&mut self.to_advance);
        self.buffer.advance(to_advance);

        match self.state {
            Initial => self.dispatch_initial().await,

            StreamDeclarationVersion(quote) => {
                self.dispatch_stream_declaration_version(quote).await
            }
            AfterDeclarationVersion => self.dispatch_after_declaration_version().await,

            StreamElementOpenName => {
                self.dispatch_stream_element_open_name(StringRing::name_continuation)
                    .await
            }
            AfterElementOpenName => self.dispatch_after_element_open_name().await,

            StreamAttributeName => {
                self.dispatch_stream_attribute_name(StringRing::name_continuation)
                    .await
            }
            AfterAttributeName => self.dispatch_after_attribute_name().await,
            StreamAttributeValue(quote) => self.dispatch_stream_attribute_value(quote).await,

            StreamElementCloseName => {
                self.dispatch_stream_element_close_name(StringRing::name_continuation)
                    .await
            }
            AfterElementCloseName => self.dispatch_after_element_close_name().await,

            StreamCData => self.dispatch_stream_cdata().await,
            State::AfterCData => self.dispatch_after_cdata().await,

            StreamReferenceNamed => {
                self.dispatch_stream_reference_named(StringRing::name_continuation)
                    .await
            }
            StreamReferenceDecimal => self.dispatch_stream_reference_decimal().await,
            StreamReferenceHex => self.dispatch_stream_reference_hex().await,
            AfterReference => self.dispatch_after_reference().await,

            StreamProcessingInstructionName => {
                self.dispatch_stream_processing_instruction_name(StringRing::name_continuation)
                    .await
            }
            AfterProcessingInstructionName => {
                self.dispatch_after_processing_instruction_name().await
            }
            StreamProcessingInstructionValue => {
                self.dispatch_stream_processing_instruction_value().await
            }
            AfterProcessingInstructionValue => {
                self.dispatch_after_processing_instruction_value().await
            }

            StreamComment => self.dispatch_stream_comment().await,
            AfterComment => self.dispatch_after_comment().await,
        }
        .transpose()
    }

    async fn dispatch_initial(&mut self) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        if self.buffer.complete().await? {
            Ok(None)
        } else if *self.buffer.consume("<![CDATA[").await? {
            self.state = StreamCData;
            self.dispatch_stream_cdata().await
        } else if *self.buffer.consume("<!--").await? {
            self.state = StreamComment;
            self.dispatch_stream_comment().await
        } else if *self.buffer.consume("<?").await? {
            if *self.buffer.consume("xml").await? {
                let n_space = self.buffer.consume_space().await?;

                if n_space == 0 {
                    // This is actually a processing instruction that starts with `xml`
                    self.state = StreamProcessingInstructionName;
                    Ok(Some(Token::ProcessingInstructionStart(Streaming::Partial(
                        "xml",
                    ))))
                } else {
                    self.buffer.require("version").await?;
                    self.buffer.require("=").await?;
                    let quote = self.buffer.require_quote().await?;

                    self.state = StreamDeclarationVersion(quote);
                    self.dispatch_stream_declaration_version(quote).await
                }
            } else {
                self.state = StreamProcessingInstructionName;
                self.dispatch_stream_processing_instruction_name(StringRing::name)
                    .await
            }
        } else if *self.buffer.consume("</").await? {
            self.state = StreamElementCloseName;
            self.dispatch_stream_element_close_name(StringRing::name)
                .await
        } else if *self.buffer.consume("<").await? {
            self.state = StreamElementOpenName;
            self.dispatch_stream_element_open_name(StringRing::name)
                .await
        } else if *self.buffer.consume("&#x").await? {
            self.state = StreamReferenceHex;
            self.dispatch_stream_reference_hex().await
        } else if *self.buffer.consume("&#").await? {
            self.state = StreamReferenceDecimal;
            self.dispatch_stream_reference_decimal().await
        } else if *self.buffer.consume("&").await? {
            self.state = StreamReferenceNamed;
            self.dispatch_stream_reference_named(StringRing::name).await
        } else if let Some(l) = self.buffer.space().await? {
            self.to_advance = l;
            let s = &self.buffer.as_str()[..l];
            Ok(Some(Space(Streaming::Complete(s))))
        } else if let Some(l) = self.buffer.char_data().await? {
            self.to_advance = l;
            let s = &self.buffer.as_str()[..l];
            Ok(Some(CharData(Streaming::Complete(s))))
        } else {
            let location = self.buffer.absolute_location();
            InvalidXml { location }.fail()
        }
    }

    async fn dispatch_after_declaration_version(&mut self) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        self.buffer.consume_space().await?;

        self.buffer.require("?>").await?;

        self.state = Initial;
        Ok(Some(DeclarationClose))
    }

    async fn dispatch_stream_declaration_version(
        &mut self,
        quote: Quote,
    ) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        let value = self.buffer.consume_until(quote).await?;

        self.to_advance = value.unify().len();

        if value.is_complete() {
            self.state = AfterDeclarationVersion;
            self.to_advance += quote.as_ref().len(); // Include the closing quote
        }

        Ok(Some(DeclarationStart(value)))
    }

    async fn dispatch_stream_element_open_name<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        self.stream_from_buffer(f, State::AfterElementOpenName, Token::ElementOpenStart)
            .await
    }

    async fn dispatch_stream_element_close_name<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        self.stream_from_buffer(f, State::AfterElementCloseName, Token::ElementClose)
            .await
    }

    async fn dispatch_after_element_open_name(&mut self) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        self.buffer.consume_space().await?;

        if *self.buffer.consume("/>").await? {
            self.state = Initial;
            Ok(Some(ElementSelfClose))
        } else if *self.buffer.consume(">").await? {
            self.state = Initial;
            Ok(Some(ElementOpenEnd))
        } else {
            self.state = StreamAttributeName;
            self.dispatch_stream_attribute_name(StringRing::name).await
        }
    }

    async fn dispatch_stream_attribute_name<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        self.stream_from_buffer(f, State::AfterAttributeName, Token::AttributeName)
            .await
    }

    async fn dispatch_after_attribute_name(&mut self) -> Result<Option<TokenS<'_>>> {
        use State::*;

        self.buffer.consume_space().await?;
        self.buffer.require("=").await?;
        self.buffer.consume_space().await?;
        let quote = self.buffer.require_quote().await?;

        self.state = StreamAttributeValue(quote);
        self.dispatch_stream_attribute_value(quote).await
    }

    async fn dispatch_stream_attribute_value(
        &mut self,
        quote: Quote,
    ) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        let value = self.buffer.consume_until(&quote).await?;

        self.to_advance = value.unify().len();

        if value.is_complete() {
            self.state = AfterElementOpenName;
            self.to_advance += quote.as_ref().len() // Include the closing quote
        }

        Ok(Some(AttributeValue(value)))
    }

    async fn dispatch_after_element_close_name(&mut self) -> Result<Option<TokenS<'_>>> {
        use State::*;

        self.buffer.consume_space().await?;
        self.buffer.require(">").await?;

        self.state = Initial;
        self.dispatch_initial().await
    }

    async fn dispatch_stream_reference_named<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        self.stream_from_buffer(f, State::AfterReference, Token::ReferenceNamed)
            .await
    }

    async fn dispatch_stream_reference_decimal(&mut self) -> Result<Option<TokenS<'_>>> {
        self.stream_from_buffer(
            StringRing::reference_decimal,
            State::AfterReference,
            Token::ReferenceDecimal,
        )
        .await
    }

    async fn dispatch_stream_reference_hex(&mut self) -> Result<Option<TokenS<'_>>> {
        self.stream_from_buffer(
            StringRing::reference_hex,
            State::AfterReference,
            Token::ReferenceHex,
        )
        .await
    }

    async fn dispatch_after_reference(&mut self) -> Result<Option<TokenS<'_>>> {
        use State::*;

        self.buffer.require(";").await?;
        self.state = Initial;
        self.dispatch_initial().await
    }

    async fn dispatch_stream_processing_instruction_name<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        self.stream_from_buffer(
            f,
            State::AfterProcessingInstructionName,
            Token::ProcessingInstructionStart,
        )
        .await
    }

    async fn dispatch_after_processing_instruction_name(&mut self) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        self.buffer.consume_space().await?;

        if *self.buffer.consume("?>").await? {
            self.state = Initial;
            Ok(Some(ProcessingInstructionEnd))
        } else {
            self.state = State::StreamProcessingInstructionValue;
            self.dispatch_stream_processing_instruction_value().await
        }
    }

    async fn dispatch_stream_processing_instruction_value(&mut self) -> Result<Option<TokenS<'_>>> {
        self.stream_from_buffer(
            StringRing::processing_instruction_value,
            State::AfterProcessingInstructionValue,
            Token::ProcessingInstructionValue,
        )
        .await
    }

    async fn dispatch_after_processing_instruction_value(&mut self) -> Result<Option<TokenS<'_>>> {
        use {State::*, Token::*};

        self.buffer.require("?>").await?;

        self.state = Initial;
        Ok(Some(ProcessingInstructionEnd))
    }

    async fn dispatch_stream_cdata(&mut self) -> Result<Option<TokenS<'_>>> {
        self.stream_from_buffer(StringRing::cdata, State::AfterCData, Token::CData)
            .await
    }

    async fn dispatch_after_cdata(&mut self) -> Result<Option<TokenS<'_>>> {
        use State::*;

        self.buffer.require("]]>").await?;

        self.state = Initial;
        self.dispatch_initial().await
    }

    async fn dispatch_stream_comment(&mut self) -> Result<Option<TokenS<'_>>> {
        self.stream_from_buffer(StringRing::comment, State::AfterComment, Token::Comment)
            .await
    }

    async fn dispatch_after_comment(&mut self) -> Result<Option<TokenS<'_>>> {
        use State::*;

        self.buffer
            .require_or_else("-->", |location| DoubleHyphenInComment { location }.build())
            .await?;

        self.state = Initial;
        self.dispatch_initial().await
    }

    // ----------

    #[inline]
    async fn stream_from_buffer<'a, Fut>(
        &'a mut self,
        f: impl FnOnce(&'a mut StringRing<S>) -> Fut,
        next_state: State,
        create: impl FnOnce(Streaming<&'a str>) -> TokenS<'a>,
    ) -> Result<Option<TokenS<'a>>>
    where
        Fut: Future<Output = Result<Streaming<&'a str>>>,
    {
        let value = f(&mut self.buffer).await?;
        self.to_advance = value.unify().len();

        if value.is_complete() {
            self.state = next_state;
        }

        Ok(Some(create(value)))
    }
}

// https://github.com/rust-lang/rust/issues/78149
#[must_use]
#[derive(Debug)]
struct MustUse<T>(T);

impl<T> std::ops::Deref for MustUse<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> std::ops::DerefMut for MustUse<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.0
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    NoMoreInputAvailable,

    #[snafu(display(
        "The {} bytes of input data, starting at byte {}, was not UTF-8",
        length,
        location,
    ))]
    InputNotUtf8 {
        location: usize,
        length: usize,
    },

    #[snafu(display(
        "Expected the token {:?} at byte {}, but it was missing",
        token,
        location
    ))]
    RequiredTokenMissing {
        token: String,
        location: usize,
    },

    #[snafu(display(
        "Expected either a single or double quote around the attribute value at byte {}",
        location,
    ))]
    ExpectedSingleOrDoubleQuote {
        location: usize,
    },

    #[snafu(display("A double hyphen is inside a comment at byte {}", location,))]
    DoubleHyphenInComment {
        location: usize,
    },

    #[snafu(display("The input data is not valid XML starting at byte {}", location))]
    InvalidXml {
        location: usize,
    },
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
