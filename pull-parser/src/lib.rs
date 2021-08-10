#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use easy_ext::ext;
use snafu::{ensure, Snafu};
use std::{io::Read, marker::PhantomData, mem, str};
use token::{IsComplete, Streaming, Token, TokenKind, UniformToken};

#[macro_use]
mod macros;

#[derive(Debug)]
struct StringRing {
    buffer: Vec<u8>,

    n_offset_bytes: usize,
    n_utf8_bytes: usize,
    n_dangling_bytes: usize,

    n_retired_bytes: usize,

    source_exhausted: bool,
}

impl StringRing {
    // The longest single token should be `standalone`, then round up
    // a bit.
    const MINIMUM_CAPACITY: usize = 16;
    const DEFAULT_CAPACITY: usize = 1024;

    fn with_capacity(capacity: usize) -> Self {
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

            source_exhausted: false,
        }
    }

    fn as_str(&self) -> &str {
        let bytes = &self.buffer[self.n_offset_bytes..][..self.n_utf8_bytes];

        // SAFETY: The range of bytes that are valid UTF-8 is checked
        // when `extend`ing the buffer and when advancing it. Checking
        // it again here leads to a massive (~1000X) slowdown.
        unsafe { str::from_utf8_unchecked(bytes) }
    }

    /// Tries to return a string with the given byte length, but if
    /// the input is exhausted, the returned string may be shorter.
    fn weak_min_str(&mut self, len: usize) -> Result<&str> {
        ensure!(
            self.n_utf8_bytes >= len || self.source_exhausted,
            NeedsMoreInput
        );
        Ok(self.as_str())
    }

    fn min_str(&mut self, len: usize) -> Result<&str> {
        if self.n_utf8_bytes < len {
            if self.source_exhausted {
                return InputExhausted.fail();
            } else {
                return NeedsMoreInput.fail();
            }
        }

        Ok(self.as_str())
    }

    fn some_str(&mut self) -> Result<&str> {
        self.min_str(1)
    }

    fn absolute_location(&self) -> usize {
        self.n_retired_bytes + self.n_offset_bytes
    }

    fn refill_using<E>(
        &mut self,
        f: impl FnOnce(&mut [u8]) -> Result<usize, E>,
    ) -> Result<Result<usize, E>> {
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

        let n_new_bytes = match f(buffer) {
            Ok(n) => n,
            Err(e) => return Ok(Err(e)),
        };

        if n_new_bytes == 0 {
            self.source_exhausted = true;
        }

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

        // SAFETY: We just calculated how many bytes of the dangling
        // bytes are valid UTF-8, so we don't need to do it again.
        unsafe {
            let s = str::from_utf8_unchecked(&dangling_bytes[..n_new_utf8_bytes]);
            for (idx, c) in s.char_indices() {
                ensure!(
                    c.is_allowed_xml_char(),
                    InvalidChar {
                        location: self.n_retired_bytes
                            + self.n_offset_bytes
                            + self.n_utf8_bytes
                            + idx,
                        length: c.len_utf8()
                    }
                )
            }
        }

        self.n_dangling_bytes -= n_new_utf8_bytes;
        self.n_utf8_bytes += n_new_utf8_bytes;

        Ok(Ok(n_new_bytes))
    }

    fn complete(&mut self) -> bool {
        self.n_utf8_bytes == 0 && self.n_dangling_bytes == 0 && self.source_exhausted
    }

    fn starts_with(&mut self, needle: &str) -> Result<bool> {
        let s = abandon!(self.weak_min_str(needle.len()));
        Ok(s.starts_with(needle))
    }

    fn advance(&mut self, n_bytes: usize) {
        // SAFETY: These help uphold the safety invariants in `as_str`
        assert!(n_bytes <= self.n_utf8_bytes);
        assert!(self.as_str().is_char_boundary(n_bytes));

        self.n_offset_bytes += n_bytes;
        self.n_utf8_bytes -= n_bytes;
    }

    fn consume(&mut self, s: impl AsRef<str>) -> Result<MustUse<bool>> {
        let s = s.as_ref();

        if abandon!(self.starts_with(s)) {
            self.advance(s.len());
            Ok(MustUse(true))
        } else {
            Ok(MustUse(false))
        }
    }

    fn consume_xml(&mut self) -> Result<MustUse<bool>> {
        let s = abandon!(self.weak_min_str(4));

        if let Some(x) = s.strip_prefix("xml") {
            let next = match x.as_bytes().split_first() {
                Some((&h, _)) => h,
                None => return Ok(MustUse(false)),
            };

            if next == b'?' || next.is_xml_space() {
                self.advance(3);
                Ok(MustUse(true))
            } else {
                Ok(MustUse(false))
            }
        } else {
            Ok(MustUse(false))
        }
    }

    /// Returns the next byte, if any are still in the valid string buffer
    fn maybe_peek_byte(&mut self) -> Option<u8> {
        self.buffer.get(self.n_offset_bytes).copied()
    }

    /// Peeks at the next byte and returns true if:
    ///
    /// - the next byte is '!' or '?', indicating a potential special tag (`<!...`, `<?...`)
    /// - the buffer is empty (no next byte currently available, so we have to check for
    ///   special tags the normal way)
    fn maybe_special_tag_start_char(&mut self) -> MustUse<bool> {
        match self.maybe_peek_byte() {
            Some(b) => MustUse(b == b'!' || b == b'?'),
            None => MustUse(true),
        }
    }

    fn require_or_else(&mut self, s: &str, e: impl FnOnce(usize) -> Error) -> Result<()> {
        if *abandon!(self.consume(s)) {
            Ok(())
        } else {
            Err(e(self.absolute_location()))
        }
    }

    fn require(&mut self, token: &'static str) -> Result<()> {
        self.require_or_else(token, |location| {
            RequiredTokenMissing {
                token: RequiredToken::from_token(token),
                location,
            }
            .build()
        })
    }

    fn require_quote(&mut self) -> Result<Quote> {
        if *abandon!(self.consume(&Quote::Double)) {
            Ok(Quote::Double)
        } else if *abandon!(self.consume(&Quote::Single)) {
            Ok(Quote::Single)
        } else {
            let location = self.absolute_location();
            ExpectedSingleOrDoubleQuote { location }.fail()
        }
    }

    fn attribute_value(&mut self, quote_style: Quote) -> Result<Streaming<usize>> {
        let s = abandon!(self.some_str());

        match memchr::memchr3(b'<', b'&', quote_style.to_ascii_char(), s.as_bytes()) {
            Some(x) => Ok(Streaming::Complete(x)),
            None => Ok(Streaming::Partial(s.len())),
        }
    }

    /// Anything that's not `<` or `&` so long as it doesn't include `]]>`
    fn char_data(&mut self) -> Result<Streaming<usize>> {
        // 3 so we will be able to tell if we start with `]]>`
        let mut s = abandon!(self.weak_min_str(3)).as_bytes();
        let mut running_offset = 0;

        loop {
            let inner = memchr::memchr3(b'<', b'&', b']', s);

            match inner {
                None => break Ok(Streaming::Partial(running_offset + s.len())),
                Some(inner_offset) => {
                    let (head, tail) = s.split_at(inner_offset);
                    if tail.starts_with(b"]") && tail.len() >= 3 {
                        if tail.starts_with(b"]]>") {
                            break Ok(Streaming::Complete(running_offset + head.len()));
                        } else {
                            running_offset += head.len() + 1; // Skip over the `]`
                            s = &tail[1..];
                        }
                    } else {
                        break Ok(Streaming::Complete(running_offset + head.len()));
                    }
                }
            }
        }
    }

    fn first_char_data(&mut self) -> Result<Option<Streaming<usize>>> {
        let v = abandon!(self.char_data());
        if *v.unify() == 0 {
            Ok(None)
        } else {
            Ok(Some(v))
        }
    }

    /// Anything that's not `]]>`
    fn cdata(&mut self) -> Result<Streaming<usize>> {
        let s = abandon!(self.min_str(3));

        match s.find("]]>") {
            Some(offset) => return Ok(Streaming::Complete(offset)),
            None => {
                // Once for each `]`
                let s = s.strip_suffix(']').unwrap_or(s);
                let s = s.strip_suffix(']').unwrap_or(s);
                Ok(Streaming::Partial(s.len()))
            }
        }
    }

    fn processing_instruction_value(&mut self) -> Result<Streaming<usize>> {
        let s = abandon!(self.min_str(2));

        match s.find("?>") {
            Some(offset) => Ok(Streaming::Complete(offset)),
            None => {
                let s = s.strip_suffix('?').unwrap_or(s);
                Ok(Streaming::Partial(s.len()))
            }
        }
    }

    fn comment(&mut self) -> Result<Streaming<usize>> {
        let s = abandon!(self.min_str(2));

        match s.find("--") {
            Some(offset) => Ok(Streaming::Complete(offset)),
            None => {
                let s = s.strip_suffix('-').unwrap_or(s);
                Ok(Streaming::Partial(s.len()))
            }
        }
    }

    // Note that space amounts can be unbounded, which means that any
    // use of this should likely occur at the beginning of a state
    // dispatch.
    fn consume_space(&mut self) -> Result<usize> {
        let s = self.as_str();

        let n_bytes_space = s
            .as_bytes()
            .iter()
            .position(|c| !c.is_xml_space())
            .unwrap_or_else(|| s.len());

        let all_space = n_bytes_space == s.len();

        self.advance(n_bytes_space);
        ensure!(!all_space, NeedsMoreInputSpace { n_bytes_space });

        Ok(n_bytes_space)
    }

    fn reference_decimal(&mut self) -> Result<Streaming<usize>> {
        // SAFETY: only checks ascii characters
        unsafe { self.while_bytes(u8::is_ascii_digit) }
    }

    fn reference_hex(&mut self) -> Result<Streaming<usize>> {
        // SAFETY: only checks ascii characters
        unsafe { self.while_bytes(u8::is_ascii_hexdigit) }
    }

    fn name(&mut self) -> Result<Streaming<usize>> {
        let s = abandon!(self.some_str());

        let mut c = s.char_indices();

        let end_idx = match c
            .next()
            .filter(|(_, c)| c.is_name_start_char())
            .map(|(i, c)| i + c.len_utf8())
        {
            Some(i) => i,
            None => return Ok(Streaming::Complete(0)),
        };

        let end_idx = c
            .take_while(|(_, c)| c.is_name_char())
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(end_idx);

        if end_idx == s.len() {
            Ok(Streaming::Partial(s.len()))
        } else {
            Ok(Streaming::Complete(end_idx))
        }
    }

    fn name_continuation(&mut self) -> Result<Streaming<usize>> {
        self.while_char(char::is_name_char)
    }

    #[inline]
    fn while_char(&mut self, predicate: impl Fn(&char) -> bool) -> Result<Streaming<usize>> {
        let s = abandon!(self.some_str());

        match matching_bytes(s, predicate) {
            offset if offset == s.len() => Ok(Streaming::Partial(s.len())),
            offset => Ok(Streaming::Complete(offset)),
        }
    }

    /// # Safety
    ///
    /// The caller has to make sure the remaining string is valid utf8
    /// (= predicate may only check for ASCII).
    #[inline]
    unsafe fn while_bytes(&mut self, predicate: impl Fn(&u8) -> bool) -> Result<Streaming<usize>> {
        let s = abandon!(self.some_str());

        match s.as_bytes().iter().position(|c| !predicate(c)) {
            None => Ok(Streaming::Partial(s.len())), // all bytes match
            Some(offset) => Ok(Streaming::Complete(offset)),
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
impl u8 {
    #[inline]
    fn is_xml_space(&self) -> bool {
        matches!(*self, b' ' | 9 | b'\r' | b'\n')
    }
}

#[ext(pub XmlCharExt)]
impl char {
    #[inline]
    fn is_allowed_xml_char(self) -> bool {
        // Sorted by how common each case is, using early exits
        match self {
            '\u{20}'..='\u{FF}' => true,
            '\u{9}' | '\u{A}' | '\u{D}' => true,
            '\u{100}'..='\u{D7FF}' | '\u{E000}'..='\u{FFFD}' | '\u{10000}'..='\u{10FFFF}' => true,
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
    fn is_name_start_char_non_ascii(self) -> bool {
        if matches!(self, '\u{C0}'..='\u{2FF}') && self != '\u{D7}' && self != '\u{F7}' {
            return true;
        }
        match self {
            '\u{370}'..='\u{37D}'
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
    fn is_name_start_char(self) -> bool {
        if self.is_name_start_char_ascii() {
            return true;
        }

        self.is_name_start_char_non_ascii()
    }

    #[inline]
    fn is_name_non_start_char_ascii(&self) -> bool {
        match self {
            '-' | '.' | '0'..='9' => true,
            _ => false,
        }
    }

    #[inline]
    fn is_name_char(&self) -> bool {
        if self.is_name_start_char_ascii() {
            return true;
        }

        if self.is_name_non_start_char_ascii() {
            return true;
        }

        if self.is_name_start_char_non_ascii() {
            return true;
        }

        match self {
            '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}' => true,
            _ => false,
        }
    }
}

#[ext(pub XmlStrExt)]
impl str {
    fn is_xml_space(&self) -> bool {
        self.as_bytes().iter().all(u8::is_xml_space)
    }
}

// There are a number of `*Space` states. Larger concepts may contain
// optional whitespace which may fill out the rest of the buffer. We
// need to be able to exit our loop, allow the user to refill the
// buffer, then resume parsing without losing our place. Each unique
// state provides a resumption point.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    Initial,

    AfterDeclarationOpen,
    AfterDeclarationOpenSpace,

    AfterDeclarationVersionAttribute,
    AfterDeclarationVersionAttributeSpace,
    AfterDeclarationVersionAttributeEquals,
    AfterDeclarationVersionAttributeEqualsSpace,
    StreamDeclarationVersion(Quote),
    AfterDeclarationVersion,
    AfterDeclarationVersionSpace,

    AfterDeclarationEncodingAttribute,
    AfterDeclarationEncodingAttributeSpace,
    AfterDeclarationEncodingAttributeEquals,
    AfterDeclarationEncodingAttributeEqualsSpace,
    StreamDeclarationEncoding(Quote),
    AfterDeclarationEncoding,
    AfterDeclarationEncodingSpace,

    AfterDeclarationStandaloneAttribute,
    AfterDeclarationStandaloneAttributeSpace,
    AfterDeclarationStandaloneAttributeEquals,
    AfterDeclarationStandaloneAttributeEqualsSpace,
    StreamDeclarationStandalone(Quote),
    AfterDeclarationStandalone,
    AfterDeclarationStandaloneSpace,

    StreamElementOpenName,
    AfterElementOpenName,
    AfterElementOpenNameRequiredSpace,
    AfterElementOpenNameSpace,

    StreamAttributeName,
    AfterAttributeName,

    AfterAttributeNameSpace,
    AfterAttributeNameEquals,
    AfterAttributeNameEqualsSpace,

    AfterAttributeOpenQuote(Quote),
    StreamAttributeValueLiteral(Quote),
    StreamAttributeValueReferenceHex(Quote),
    StreamAttributeValueReferenceDecimal(Quote),
    StreamAttributeValueReferenceNamed(Quote),
    AfterAttributeValueReference(Quote),

    StreamElementCloseName,
    AfterElementCloseName,
    AfterElementCloseNameSpace,

    StreamReferenceNamed,
    StreamReferenceDecimal,
    StreamReferenceHex,
    AfterReference,

    StreamProcessingInstructionName,
    AfterProcessingInstructionName,
    AfterProcessingInstructionNameRequiredSpace,
    AfterProcessingInstructionNameSpace,
    StreamProcessingInstructionValue,
    AfterProcessingInstructionValue,

    StreamCharData,
    StreamCData,
    AfterCData,

    StreamComment,
    AfterComment,
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

    fn to_ascii_char(&self) -> u8 {
        match self {
            Self::Single => b'\'',
            Self::Double => b'"',
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

type IndexToken = UniformToken<Streaming<usize>>;
type RollbackState = (usize, usize);

#[derive(Debug)]
pub struct CoreParser {
    buffer: StringRing,
    state: State,
    to_advance: usize,
    rollback_to: RollbackState,
}

macro_rules! dispatch_eq_value {
    ($k:ident) => {
        paste::paste! {
            fn [<dispatch_after_ $k>](&mut self) -> Result<Option<IndexToken>> {
                self.consume_space(
                    State::[<After $k:camel Space>],
                    Self::[<dispatch_after_ $k _space>],
                )
            }

            fn [<dispatch_after_ $k _space>](&mut self) -> Result<Option<IndexToken>> {
                use State::*;

                self.buffer.require("=")?;

                self.ratchet([<After $k:camel Equals>]);
                self.[<dispatch_after_ $k _equals>]()
            }

            fn [<dispatch_after_ $k _equals>](&mut self) -> Result<Option<IndexToken>> {
                self.consume_space(
                    State::[<After $k:camel EqualsSpace>],
                    Self::[<dispatch_after_ $k _equals_space>],
                )
            }
        }
    };
}

impl CoreParser {
    pub fn new() -> Self {
        Self::with_capacity(StringRing::DEFAULT_CAPACITY)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        CoreParser {
            buffer: StringRing::with_capacity(capacity),
            state: State::Initial,
            to_advance: 0,
            rollback_to: (0, 0),
        }
    }

    pub fn as_str(&self) -> &str {
        self.buffer.as_str()
    }

    pub fn exchange(&self, idx: usize) -> &str {
        &self.as_str()[..idx]
    }

    pub fn refill_using<E>(
        &mut self,
        f: impl FnOnce(&mut [u8]) -> Result<usize, E>,
    ) -> Result<Result<usize, E>> {
        self.buffer.refill_using(f)
    }

    // When we transition state without returning yet, we need to
    // update where we should rollback to.
    //
    // Possible future perf: should only be needed when we immediately
    // jump to another dispatch function. We also don't need to
    // re-set the state at the start of `next`.
    fn ratchet(&mut self, state: State) {
        self.state = state;

        self.rollback_to = (self.buffer.n_offset_bytes, self.buffer.n_utf8_bytes)
    }

    fn rollback(&mut self) {
        self.buffer.n_offset_bytes = self.rollback_to.0;
        self.buffer.n_utf8_bytes = self.rollback_to.1;
    }

    #[inline]
    pub fn next(&mut self) -> Option<Result<IndexToken>> {
        use State::*;

        let to_advance = mem::take(&mut self.to_advance);
        self.buffer.advance(to_advance);

        if self.buffer.complete() {
            return match self.finish() {
                Ok(()) => None,
                Err(e) => Some(Err(e)),
            };
        }

        self.ratchet(self.state);

        let token = match self.state {
            Initial => self.dispatch_initial(),

            AfterDeclarationOpen => self.dispatch_after_declaration_open(),
            AfterDeclarationOpenSpace => self.dispatch_after_declaration_open_space(),

            AfterDeclarationVersionAttribute => self.dispatch_after_declaration_version_attribute(),
            AfterDeclarationVersionAttributeSpace => {
                self.dispatch_after_declaration_version_attribute_space()
            }
            AfterDeclarationVersionAttributeEquals => {
                self.dispatch_after_declaration_version_attribute_equals()
            }
            AfterDeclarationVersionAttributeEqualsSpace => {
                self.dispatch_after_declaration_version_attribute_equals_space()
            }
            StreamDeclarationVersion(quote) => self.dispatch_stream_declaration_version(quote),
            AfterDeclarationVersion => self.dispatch_after_declaration_version(),
            AfterDeclarationVersionSpace => self.dispatch_after_declaration_version_space(),

            AfterDeclarationEncodingAttribute => {
                self.dispatch_after_declaration_encoding_attribute()
            }
            AfterDeclarationEncodingAttributeSpace => {
                self.dispatch_after_declaration_encoding_attribute_space()
            }
            AfterDeclarationEncodingAttributeEquals => {
                self.dispatch_after_declaration_encoding_attribute_equals()
            }
            AfterDeclarationEncodingAttributeEqualsSpace => {
                self.dispatch_after_declaration_encoding_attribute_equals_space()
            }
            StreamDeclarationEncoding(quote) => self.dispatch_stream_declaration_encoding(quote),
            AfterDeclarationEncoding => self.dispatch_after_declaration_encoding(),
            AfterDeclarationEncodingSpace => self.dispatch_after_declaration_encoding_space(),

            AfterDeclarationStandaloneAttribute => {
                self.dispatch_after_declaration_standalone_attribute()
            }
            AfterDeclarationStandaloneAttributeSpace => {
                self.dispatch_after_declaration_standalone_attribute_space()
            }
            AfterDeclarationStandaloneAttributeEquals => {
                self.dispatch_after_declaration_standalone_attribute_equals()
            }
            AfterDeclarationStandaloneAttributeEqualsSpace => {
                self.dispatch_after_declaration_standalone_attribute_equals_space()
            }
            StreamDeclarationStandalone(quote) => {
                self.dispatch_stream_declaration_standalone(quote)
            }
            AfterDeclarationStandalone => self.dispatch_after_declaration_standalone(),
            AfterDeclarationStandaloneSpace => self.dispatch_after_declaration_standalone_space(),

            StreamElementOpenName => {
                self.dispatch_stream_element_open_name(StringRing::name_continuation)
            }
            AfterElementOpenName => self.dispatch_after_element_open_name(),
            AfterElementOpenNameRequiredSpace => {
                self.dispatch_after_element_open_name_required_space()
            }
            AfterElementOpenNameSpace => self.dispatch_after_element_open_name_space(),

            StreamAttributeName => {
                self.dispatch_stream_attribute_name(StringRing::name_continuation)
            }
            AfterAttributeName => self.dispatch_after_attribute_name(),
            AfterAttributeNameSpace => self.dispatch_after_attribute_name_space(),
            AfterAttributeNameEquals => self.dispatch_after_attribute_name_equals(),
            AfterAttributeNameEqualsSpace => self.dispatch_after_attribute_name_equals_space(),
            AfterAttributeOpenQuote(quote) => self.dispatch_after_attribute_open_quote(quote),
            StreamAttributeValueLiteral(quote) => {
                self.dispatch_stream_attribute_value_literal(quote)
            }
            StreamAttributeValueReferenceHex(quote) => {
                self.dispatch_stream_attribute_value_reference_hex(quote)
            }
            StreamAttributeValueReferenceDecimal(quote) => {
                self.dispatch_stream_attribute_value_reference_decimal(quote)
            }
            StreamAttributeValueReferenceNamed(quote) => self
                .dispatch_stream_attribute_value_reference_named(
                    quote,
                    StringRing::name_continuation,
                ),
            AfterAttributeValueReference(quote) => {
                self.dispatch_after_attribute_value_reference(quote)
            }

            StreamElementCloseName => {
                self.dispatch_stream_element_close_name(StringRing::name_continuation)
            }
            AfterElementCloseName => self.dispatch_after_element_close_name(),
            AfterElementCloseNameSpace => self.dispatch_after_element_close_name_space(),

            StreamCharData => self.dispatch_stream_char_data(),
            StreamCData => self.dispatch_stream_cdata(),
            AfterCData => self.dispatch_after_cdata(),

            StreamReferenceNamed => {
                self.dispatch_stream_reference_named(StringRing::name_continuation)
            }
            StreamReferenceDecimal => self.dispatch_stream_reference_decimal(),
            StreamReferenceHex => self.dispatch_stream_reference_hex(),
            AfterReference => self.dispatch_after_reference(),

            StreamProcessingInstructionName => {
                self.dispatch_stream_processing_instruction_name(StringRing::name_continuation)
            }
            AfterProcessingInstructionName => self.dispatch_after_processing_instruction_name(),
            AfterProcessingInstructionNameRequiredSpace => {
                self.dispatch_after_processing_instruction_name_required_space()
            }
            AfterProcessingInstructionNameSpace => {
                self.dispatch_after_processing_instruction_name_space()
            }
            StreamProcessingInstructionValue => self.dispatch_stream_processing_instruction_value(),
            AfterProcessingInstructionValue => self.dispatch_after_processing_instruction_value(),

            StreamComment => self.dispatch_stream_comment(),
            AfterComment => self.dispatch_after_comment(),
        };

        if matches!(token, Err(Error::NeedsMoreInput)) {
            self.rollback();
        }

        token.transpose()
    }

    fn finish(&mut self) -> Result<()> {
        ensure!(
            matches!(self.state, State::Initial | State::StreamCharData),
            IncompleteXml
        );
        Ok(())
    }

    fn dispatch_initial(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("<")? {
            if *self.buffer.consume("/")? {
                self.ratchet(StreamElementCloseName);
                return self.dispatch_stream_element_close_name(StringRing::name);
            }

            if *self.buffer.maybe_special_tag_start_char() {
                if *self.buffer.consume("![CDATA[")? {
                    self.ratchet(StreamCData);
                    return self.dispatch_stream_cdata();
                }
                if *self.buffer.consume("!--")? {
                    self.ratchet(StreamComment);
                    return self.dispatch_stream_comment();
                }
                if *self.buffer.consume("?")? {
                    if *self.buffer.consume_xml()? {
                        self.ratchet(AfterDeclarationOpen);
                        return self.dispatch_after_declaration_open();
                    } else {
                        self.ratchet(StreamProcessingInstructionName);
                        return self.dispatch_stream_processing_instruction_name(StringRing::name);
                    }
                }
            }

            // regular open tag
            self.ratchet(StreamElementOpenName);
            return self.dispatch_stream_element_open_name(StringRing::name);
        }

        if let Some(v) = self.buffer.first_char_data()? {
            self.to_advance = *v.unify();

            if !v.is_complete() {
                self.state = StreamCharData;
            }

            Ok(Some(CharData(v)))
        } else if *self.buffer.consume("&#x")? {
            self.ratchet(StreamReferenceHex);
            self.dispatch_stream_reference_hex()
        } else if *self.buffer.consume("&#")? {
            self.ratchet(StreamReferenceDecimal);
            self.dispatch_stream_reference_decimal()
        } else if *self.buffer.consume("&")? {
            self.ratchet(StreamReferenceNamed);
            self.dispatch_stream_reference_named(StringRing::name)
        } else {
            let location = self.buffer.absolute_location();
            InvalidXml { location }.fail()
        }
    }

    fn dispatch_after_declaration_open(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterDeclarationOpenSpace,
            Self::dispatch_after_declaration_open_space,
        )
    }

    fn dispatch_after_declaration_open_space(&mut self) -> Result<Option<IndexToken>> {
        self.buffer.require("version")?;
        self.ratchet(State::AfterDeclarationVersionAttribute);
        self.dispatch_after_declaration_version_attribute()
    }

    dispatch_eq_value!(declaration_version_attribute);

    fn dispatch_after_declaration_version_attribute_equals_space(
        &mut self,
    ) -> Result<Option<IndexToken>> {
        let quote = self.buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationVersion(quote));
        return self.dispatch_stream_declaration_version(quote);
    }

    fn dispatch_stream_declaration_version(&mut self, quote: Quote) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        let value = self.buffer.attribute_value(quote)?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterDeclarationVersion);
            self.to_advance += quote.as_ref().len(); // Include the closing quote
        }

        Ok(Some(DeclarationStart(value)))
    }

    fn dispatch_after_declaration_version(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterDeclarationVersionSpace,
            Self::dispatch_after_declaration_version_space,
        )
    }

    fn dispatch_after_declaration_version_space(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        // TODO: this should require that we've seen a space in order to be allowed
        if *self.buffer.consume("encoding")? {
            self.ratchet(AfterDeclarationEncodingAttribute);
            self.dispatch_after_declaration_encoding_attribute()
        } else if *self.buffer.consume("standalone")? {
            self.ratchet(AfterDeclarationStandaloneAttribute);
            self.dispatch_after_declaration_standalone_attribute()
        } else {
            self.buffer.require("?>")?;

            self.ratchet(Initial);
            Ok(Some(DeclarationClose))
        }
    }

    dispatch_eq_value!(declaration_encoding_attribute);

    fn dispatch_after_declaration_encoding_attribute_equals_space(
        &mut self,
    ) -> Result<Option<IndexToken>> {
        let quote = self.buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationEncoding(quote));
        self.dispatch_stream_declaration_encoding(quote)
    }

    fn dispatch_stream_declaration_encoding(&mut self, quote: Quote) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        let value = self.buffer.attribute_value(quote)?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterDeclarationEncoding);
            self.to_advance += quote.as_ref().len(); // Include the closing quote
        }

        Ok(Some(DeclarationEncoding(value)))
    }

    fn dispatch_after_declaration_encoding(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterDeclarationEncodingSpace,
            Self::dispatch_after_declaration_encoding_space,
        )
    }

    fn dispatch_after_declaration_encoding_space(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("standalone")? {
            self.ratchet(AfterDeclarationStandaloneAttribute);
            self.dispatch_after_declaration_standalone_attribute()
        } else {
            self.buffer.require("?>")?;

            self.ratchet(Initial);
            Ok(Some(DeclarationClose))
        }
    }

    dispatch_eq_value!(declaration_standalone_attribute);

    fn dispatch_after_declaration_standalone_attribute_equals_space(
        &mut self,
    ) -> Result<Option<IndexToken>> {
        let quote = self.buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationStandalone(quote));
        self.dispatch_stream_declaration_standalone(quote)
    }

    fn dispatch_stream_declaration_standalone(
        &mut self,
        quote: Quote,
    ) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        let value = self.buffer.attribute_value(quote)?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterDeclarationStandalone);
            self.to_advance += quote.as_ref().len(); // Include the closing quote
        }

        Ok(Some(DeclarationStandalone(value)))
    }

    fn dispatch_after_declaration_standalone(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterDeclarationStandaloneSpace,
            Self::dispatch_after_declaration_standalone_space,
        )
    }

    fn dispatch_after_declaration_standalone_space(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        self.buffer.require("?>")?;

        self.ratchet(Initial);
        Ok(Some(DeclarationClose))
    }

    fn dispatch_stream_element_open_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(f, State::AfterElementOpenName, Token::ElementOpenStart)
    }

    fn dispatch_stream_element_close_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(f, State::AfterElementCloseName, Token::ElementClose)
    }

    fn dispatch_after_element_open_name(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("/>")? {
            self.ratchet(Initial);
            Ok(Some(ElementSelfClose))
        } else if *self.buffer.consume(">")? {
            self.ratchet(Initial);
            Ok(Some(ElementOpenEnd))
        } else {
            self.require_space(
                AfterElementOpenNameRequiredSpace,
                Self::dispatch_after_element_open_name_required_space,
            )
        }
    }

    fn dispatch_after_element_open_name_required_space(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterElementOpenNameSpace,
            Self::dispatch_after_element_open_name_space,
        )
    }

    fn dispatch_after_element_open_name_space(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("/>")? {
            self.ratchet(Initial);
            Ok(Some(ElementSelfClose))
        } else if *self.buffer.consume(">")? {
            self.ratchet(Initial);
            Ok(Some(ElementOpenEnd))
        } else {
            self.ratchet(StreamAttributeName);
            self.dispatch_stream_attribute_name(StringRing::name)
        }
    }

    fn dispatch_stream_attribute_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(f, State::AfterAttributeName, Token::AttributeStart)
    }

    dispatch_eq_value!(attribute_name);

    fn dispatch_after_attribute_name_equals_space(&mut self) -> Result<Option<IndexToken>> {
        use State::*;

        let quote = self.buffer.require_quote()?;
        self.ratchet(AfterAttributeOpenQuote(quote));
        self.dispatch_after_attribute_open_quote(quote)
    }

    fn dispatch_after_attribute_open_quote(&mut self, quote: Quote) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume(quote)? {
            self.ratchet(AfterElementOpenName);
            Ok(Some(AttributeValueEnd))
        } else if *self.buffer.consume("&#x")? {
            self.ratchet(StreamAttributeValueReferenceHex(quote));
            self.dispatch_stream_attribute_value_reference_hex(quote)
        } else if *self.buffer.consume("&#")? {
            self.ratchet(StreamAttributeValueReferenceDecimal(quote));
            self.dispatch_stream_attribute_value_reference_decimal(quote)
        } else if *self.buffer.consume("&")? {
            self.ratchet(StreamAttributeValueReferenceNamed(quote));
            self.dispatch_stream_attribute_value_reference_named(quote, StringRing::name)
        } else if self.buffer.starts_with("<")? {
            InvalidCharacterInAttribute {
                location: self.buffer.absolute_location(),
            }
            .fail()
        } else {
            self.ratchet(StreamAttributeValueLiteral(quote));
            self.dispatch_stream_attribute_value_literal(quote)
        }
    }

    // -- todo: copy-pastad
    fn dispatch_stream_attribute_value_reference_named(
        &mut self,
        quote: Quote,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            f,
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceNamed,
        )
    }

    fn dispatch_stream_attribute_value_reference_decimal(
        &mut self,
        quote: Quote,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            StringRing::reference_decimal,
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceDecimal,
        )
    }

    fn dispatch_stream_attribute_value_reference_hex(
        &mut self,
        quote: Quote,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            StringRing::reference_hex,
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceHex,
        )
    }

    fn dispatch_after_attribute_value_reference(
        &mut self,
        quote: Quote,
    ) -> Result<Option<IndexToken>> {
        use State::*;

        self.buffer.require(";")?;
        self.ratchet(AfterAttributeOpenQuote(quote));
        self.dispatch_after_attribute_open_quote(quote)
    }
    // ---

    fn dispatch_stream_attribute_value_literal(
        &mut self,
        quote: Quote,
    ) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        let value = self.buffer.attribute_value(quote)?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterAttributeOpenQuote(quote));
        }

        Ok(Some(AttributeValueLiteral(value)))
    }

    fn dispatch_after_element_close_name(&mut self) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterElementCloseNameSpace,
            Self::dispatch_after_element_close_name_space,
        )
    }

    fn dispatch_after_element_close_name_space(&mut self) -> Result<Option<IndexToken>> {
        use State::*;

        self.buffer.require(">")?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_reference_named(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(f, State::AfterReference, Token::ReferenceNamed)
    }

    fn dispatch_stream_reference_decimal(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            StringRing::reference_decimal,
            State::AfterReference,
            Token::ReferenceDecimal,
        )
    }

    fn dispatch_stream_reference_hex(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            StringRing::reference_hex,
            State::AfterReference,
            Token::ReferenceHex,
        )
    }

    fn dispatch_after_reference(&mut self) -> Result<Option<IndexToken>> {
        use State::*;

        self.buffer.require(";")?;
        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_processing_instruction_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            f,
            State::AfterProcessingInstructionName,
            Token::ProcessingInstructionStart,
        )
    }

    fn dispatch_after_processing_instruction_name(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("?>")? {
            self.ratchet(Initial);
            Ok(Some(ProcessingInstructionEnd))
        } else {
            self.require_space(
                AfterProcessingInstructionNameRequiredSpace,
                Self::dispatch_after_processing_instruction_name_required_space,
            )
        }
    }

    fn dispatch_after_processing_instruction_name_required_space(
        &mut self,
    ) -> Result<Option<IndexToken>> {
        self.consume_space(
            State::AfterProcessingInstructionNameSpace,
            Self::dispatch_after_processing_instruction_name_space,
        )
    }

    fn dispatch_after_processing_instruction_name_space(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        if *self.buffer.consume("?>")? {
            self.ratchet(Initial);
            Ok(Some(ProcessingInstructionEnd))
        } else {
            self.ratchet(StreamProcessingInstructionValue);
            self.dispatch_stream_processing_instruction_value()
        }
    }

    fn dispatch_stream_processing_instruction_value(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(
            StringRing::processing_instruction_value,
            State::AfterProcessingInstructionValue,
            Token::ProcessingInstructionValue,
        )
    }

    fn dispatch_after_processing_instruction_value(&mut self) -> Result<Option<IndexToken>> {
        use {State::*, Token::*};

        self.buffer.require("?>")?;

        self.ratchet(Initial);
        Ok(Some(ProcessingInstructionEnd))
    }

    fn dispatch_stream_char_data(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(StringRing::char_data, State::Initial, Token::CharData)
    }

    fn dispatch_stream_cdata(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(StringRing::cdata, State::AfterCData, Token::CData)
    }

    fn dispatch_after_cdata(&mut self) -> Result<Option<IndexToken>> {
        use State::*;

        self.buffer.require("]]>")?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_comment(&mut self) -> Result<Option<IndexToken>> {
        self.stream_from_buffer(StringRing::comment, State::AfterComment, Token::Comment)
    }

    fn dispatch_after_comment(&mut self) -> Result<Option<IndexToken>> {
        use State::*;

        self.buffer
            .require_or_else("-->", |location| DoubleHyphenInComment { location }.build())?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    // ----------

    fn require_space(
        &mut self,
        next_state: State,
        next_state_fn: impl FnOnce(&mut Self) -> Result<Option<IndexToken>>,
    ) -> Result<Option<IndexToken>> {
        match self.buffer.consume_space() {
            Ok(0) => RequiredSpaceMissing {
                location: self.buffer.absolute_location(),
            }
            .fail(),
            Ok(_) => {
                self.ratchet(next_state);
                next_state_fn(self)
            }
            Err(e @ Error::NeedsMoreInputSpace { n_bytes_space: 0 }) => Err(e),
            Err(e @ Error::NeedsMoreInputSpace { .. }) => {
                self.ratchet(next_state);
                Err(e)
            }
            Err(e) => Err(e),
        }
    }

    fn consume_space(
        &mut self,
        next_state: State,
        next_state_fn: impl FnOnce(&mut Self) -> Result<Option<IndexToken>>,
    ) -> Result<Option<IndexToken>> {
        self.buffer.consume_space()?;

        self.ratchet(next_state);
        next_state_fn(self)
    }

    #[inline]
    fn stream_from_buffer(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
        next_state: State,
        create: impl FnOnce(Streaming<usize>) -> IndexToken,
    ) -> Result<Option<IndexToken>> {
        let value = f(&mut self.buffer)?;
        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(next_state);
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

#[derive(Debug)]
pub enum RequiredToken {
    Version,
    Equals,
    QuestionMarkClosingAngleBracket,
    ClosingAngleBracket,
    Semicolon,
    CDataEnd,
}

impl RequiredToken {
    fn from_token(s: &'static str) -> Self {
        match s {
            "version" => Self::Version,
            "=" => Self::Equals,
            "?>" => Self::QuestionMarkClosingAngleBracket,
            ">" => Self::ClosingAngleBracket,
            ";" => Self::Semicolon,
            "]]>" => Self::CDataEnd,
            _ => panic!("unknown token"),
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    NeedsMoreInput,
    // This is used to avoid performing a state rollback
    NeedsMoreInputSpace {
        n_bytes_space: usize,
    },
    InputExhausted,

    #[snafu(display(
        "The {} bytes of input data, starting at byte {}, are not allowed in XML",
        length,
        location,
    ))]
    InvalidChar {
        location: usize,
        length: usize,
    },

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
        token: RequiredToken,
        location: usize,
    },

    #[snafu(display("Required space but it was missing"))]
    RequiredSpaceMissing {
        location: usize,
    },

    #[snafu(display(
        "Expected either a single or double quote around the attribute value at byte {}",
        location,
    ))]
    ExpectedSingleOrDoubleQuote {
        location: usize,
    },

    #[snafu(display("An invalid character is inside an attribute at byte {}", location))]
    InvalidCharacterInAttribute {
        location: usize,
    },

    #[snafu(display("A double hyphen is inside a comment at byte {}", location,))]
    DoubleHyphenInComment {
        location: usize,
    },

    #[snafu(display("The input data did not end in a valid state"))]
    IncompleteXml,

    #[snafu(display("The input data is not valid XML starting at byte {}", location))]
    InvalidXml {
        location: usize,
    },
}

impl Error {
    fn needs_more_input(&self) -> bool {
        matches!(
            self,
            Error::NeedsMoreInput | Error::NeedsMoreInputSpace { .. }
        )
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
pub struct Parser<R> {
    source: R,
    parser: CoreParser,
    exhausted: bool,
}

impl<R> Parser<R>
where
    R: Read,
{
    pub fn new(source: R) -> Self {
        Self::with_buffer_capacity(source, StringRing::DEFAULT_CAPACITY)
    }

    pub fn with_buffer_capacity(source: R, capacity: usize) -> Self {
        Self {
            source,
            parser: CoreParser::with_capacity(capacity),
            exhausted: false,
        }
    }

    // This method (and similar methods that return `usize` or other
    // non-reference types) are a workaround for the current
    // limitations of the borrow checker. If Polonius is ever merged,
    // this can be simplified.
    pub fn next_index(&mut self) -> Option<Result<IndexToken>> {
        let Self {
            parser,
            source,
            exhausted,
        } = self;

        loop {
            match parser.next() {
                None => { /* Get more data */ }
                Some(Err(e)) if e.needs_more_input() => { /* Get more data */ }
                Some(Err(e)) => break Some(Err(e)),
                Some(Ok(v)) => break Some(Ok(v)),
            }

            let n_new_bytes = parser.refill_using(|buf| source.read(buf));

            match n_new_bytes {
                Ok(Ok(0)) if *exhausted => {
                    return match parser.finish() {
                        Ok(()) => None,
                        Err(e) => Some(Err(e)),
                    }
                }
                Ok(Ok(0)) => {
                    *exhausted = true;
                    continue;
                }
                Ok(Ok(_)) => continue,
                Ok(Err(e)) => panic!("Report this: {}", e),
                Err(e) => return Some(Err(e)),
            }
        }
    }

    pub fn next(&mut self) -> Option<Result<UniformToken<Streaming<&str>>>> {
        let v = self.next_index();
        v.map(move |r| r.map(move |s| s.map(move |t| t.map(move |idx| self.parser.exchange(idx)))))
    }
}

macro_rules! fuse_invoke {
    ($inner_macro:ident) => {
        $inner_macro! {
            fuse DeclarationStart,
            fuse DeclarationEncoding,
            fuse DeclarationStandalone,
            pass DeclarationClose,
            fuse ElementOpenStart,
            pass ElementOpenEnd,
            pass ElementSelfClose,
            fuse ElementClose,
            fuse AttributeStart,
            stream AttributeValueLiteral,
            stream AttributeValueReferenceNamed,
            fuse AttributeValueReferenceDecimal,
            fuse AttributeValueReferenceHex,
            pass AttributeValueEnd,
            stream CharData,
            stream CData,
            stream ReferenceNamed,
            fuse ReferenceDecimal,
            fuse ReferenceHex,
            fuse ProcessingInstructionStart,
            stream ProcessingInstructionValue,
            pass ProcessingInstructionEnd,
            stream Comment,
        }
    };
}

#[derive(Debug, Copy, Clone)]
pub enum FusedIndex {
    Buffered,
    Direct(usize),
}

#[derive(Debug, Copy, Clone)]
pub struct FusedIndexKind(());

macro_rules! fused_index_kind {
    ($($tt:tt $name:ident,)*) => { $(fused_index_kind! { @type $tt $name })* };

    (@type pass $name:ident) => { };
    (@type fuse $name:ident) => { type $name = FusedIndex; };
    (@type stream $name:ident) => { type $name = Streaming<usize>; };
}

impl TokenKind for FusedIndexKind {
    fuse_invoke!(fused_index_kind);
}

pub type FusedIndexToken = Token<FusedIndexKind>;

#[derive(Debug, Copy, Clone)]
pub struct FusedKind<'a>(PhantomData<&'a str>);

macro_rules! fused_kind {
    ($($tt:tt $name:ident,)*) => { $(fused_kind! { @type $tt $name })* };

    (@type pass $name:ident) => { };
    (@type fuse $name:ident) => { type $name = &'a str; };
    (@type stream $name:ident) => { type $name = Streaming<&'a str>; };
}

impl<'a> TokenKind for FusedKind<'a> {
    fuse_invoke!(fused_kind);
}

pub type FusedToken<'a> = Token<FusedKind<'a>>;

trait Exchange {
    fn exchange(&self, idx: usize) -> &str;
}

impl Exchange for CoreParser {
    fn exchange(&self, idx: usize) -> &str {
        Self::exchange(self, idx)
    }
}

#[derive(Debug, Default)]
struct FuseCore {
    buffer: String,
    current: Option<UniformToken<()>>,
}

impl FuseCore {
    fn push(&mut self, t: IndexToken, parser: &impl Exchange) -> Option<FusedIndexToken> {
        use {FusedIndex::*, Streaming::*, Token::*};

        let Self { buffer, current } = self;

        macro_rules! push_match {
            ($($tt:tt $name:ident,)*) => {
                match t {
                    $( push_match!(@pat $tt $name s) => push_match!(@arm $tt $name s), )*
                }
            };

            (@pat pass $name:ident $s:ident) => { $name };
            (@arm pass $name:ident $s:ident) => { Some($name) };

            (@pat fuse $name:ident $s:ident) => { $name($s) };
            (@arm fuse $name:ident $s:ident) => {
                match ($s, *current) {
                    (Partial(v), None) => {
                        *current = Some($name(()));
                        buffer.clear();
                        buffer.push_str(parser.exchange(v));
                        None
                    }
                    (Partial(v), Some($name(()))) => {
                        buffer.push_str(parser.exchange(v));
                        None
                    }
                    (Complete(v), None) => Some($name(Direct(v))),
                    (Complete(v), Some($name(()))) => {
                        *current = None;
                        buffer.push_str(parser.exchange(v));
                        Some($name(Buffered))
                    }
                    (p, Some(c)) => unreachable!("While processing {:?}, had a cached {:?}", p, c),
                }
            };

            (@pat stream $name:ident $s:ident) => { $name($s) };
            (@arm stream $name:ident $s:ident) => {
                match ($s, current) {
                    (v, None) => Some($name(v)),
                    (p, Some(c)) => unreachable!("While processing {:?}, had a cached {:?}", p, c),
                }
            };
        }

        fuse_invoke!(push_match)
    }

    fn finish(&mut self) -> Result<Option<FusedIndexToken>, FuseError> {
        match self.current.take() {
            Some(_) => Incomplete.fail(),
            None => Ok(None),
        }
    }
}

#[derive(Debug)]
pub struct Fuse<R> {
    inner: Parser<R>,
    core: FuseCore,
}

impl<R> Fuse<R>
where
    R: Read,
{
    pub fn new(inner: Parser<R>) -> Self {
        Self {
            inner,
            core: Default::default(),
        }
    }

    pub fn next_index(&mut self) -> Option<Result<FusedIndexToken, FuseError>> {
        let Self { inner, core } = self;
        while let Some(t) = inner.next_index() {
            match t {
                Ok(t) => {
                    if let Some(t) = core.push(t, &inner.parser) {
                        return Some(Ok(t));
                    }
                }
                Err(e) => return Some(Err(e.into())),
            }
        }

        match core.finish() {
            Ok(v) => v.map(Ok),
            Err(e) => return Some(Err(e)),
        }
    }

    pub fn next(&mut self) -> Option<Result<FusedToken<'_>, FuseError>> {
        let v = self.next_index();
        v.map(move |r| r.map(move |t| self.exchange(t)))
    }

    pub fn exchange(&self, token: FusedIndexToken) -> FusedToken<'_> {
        use {FusedIndex::*, Token::*};

        macro_rules! exchange_match {
            ($($tt:tt $name:ident,)*) => {
                match token {
                    $( exchange_match!(@pat $tt $name s) => exchange_match!(@arm $tt $name s), )*
                }
            };

            (@pat pass $name:ident $s:ident) => { $name };
            (@arm pass $name:ident $s:ident) => { $name };

            (@pat fuse $name:ident $s:ident) => { $name($s) };
            (@arm fuse $name:ident $s:ident) => {
                match $s {
                    Buffered => $name(&*self.core.buffer),
                    Direct(idx) => $name(self.inner.parser.exchange(idx)),
                }
            };

            (@pat stream $name:ident $s:ident) => { $name($s) };
            (@arm stream $name:ident $s:ident) => { $name($s.map(|i| self.inner.parser.exchange(i))) };
        }

        fuse_invoke!(exchange_match)
    }
}

#[derive(Debug, Snafu)]
pub enum FuseError {
    Incomplete,

    #[snafu(context(false))]
    Parsing {
        source: Error,
    },
}

#[cfg(test)]
mod test {
    use super::*;
    use {Streaming::*, Token::*};

    type BoxError = Box<dyn std::error::Error>;
    type Result<T = (), E = BoxError> = std::result::Result<T, E>;

    type OwnedTokens = Vec<UniformToken<Streaming<String>>>;

    #[derive(Debug)]
    struct Expected<T>(T);
    use Expected as expect;

    trait Assertion<Actual> {
        type Error;

        fn assert(self, actual: Actual) -> Result<(), Self::Error>;
    }

    impl<T> Expected<T> {
        #[track_caller]
        fn to<A>(self, assertion: A) -> Result<(), A::Error>
        where
            A: Assertion<T>,
        {
            assertion.assert(self.0)
        }

        fn with<F, U>(self, modifier: F) -> Expected<U>
        where
            F: FnOnce(T) -> U,
        {
            Expected(modifier(self.0))
        }
    }

    // Would prefer that this be `IntoParser`, but that seems to run
    // into https://github.com/rust-lang/rust/issues/70263
    trait TryIntoTokens {
        type Error;

        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error>;
    }

    impl<'a> TryIntoTokens for &'a str {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            self.as_bytes().try_into_tokens()
        }
    }

    impl<'a> TryIntoTokens for &'a [u8] {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            Parser::with_buffer_capacity(self, StringRing::DEFAULT_CAPACITY).collect_owned()
        }
    }

    struct WithCapacity<T>(T, usize);

    fn capacity<'a, T>(c: usize) -> impl FnOnce(T) -> WithCapacity<T> {
        move |v| WithCapacity(v, c)
    }

    fn minimum_capacity<T>(v: T) -> WithCapacity<T> {
        WithCapacity(v, StringRing::MINIMUM_CAPACITY)
    }

    impl<'a> TryIntoTokens for WithCapacity<&'a str> {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            WithCapacity(self.0.as_bytes(), self.1).try_into_tokens()
        }
    }

    impl<'a> TryIntoTokens for WithCapacity<&'a [u8]> {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            Parser::with_buffer_capacity(self.0, self.1).collect_owned()
        }
    }

    struct BeParsedAsAssertion<const N: usize>([UniformToken<Streaming<&'static str>>; N]);
    use BeParsedAsAssertion as be_parsed_as;

    impl<Actual, const N: usize> Assertion<Actual> for BeParsedAsAssertion<N>
    where
        Actual: TryIntoTokens,
        BoxError: From<Actual::Error>,
    {
        type Error = BoxError;

        #[track_caller]
        fn assert(self, actual: Actual) -> Result<(), Self::Error> {
            let tokens = actual.try_into_tokens()?;
            assert_eq!(tokens, self.0);
            Ok(())
        }
    }

    struct ParseAssertion;
    use ParseAssertion as parse;

    impl<Actual> Assertion<Actual> for ParseAssertion
    where
        Actual: TryIntoTokens<Error = Error>,
    {
        type Error = Error;

        #[track_caller]
        fn assert(self, actual: Actual) -> Result<(), Self::Error> {
            actual.try_into_tokens().map(drop)
        }
    }

    struct FailParsingWithAssertion(fn(Result<OwnedTokens, Error>));

    macro_rules! fail_parsing_with {
        ($p:pat) => {{
            FailParsingWithAssertion(|actual| {
                assert!(
                    matches!(actual, Err($p)),
                    "Expected {}, but got {:?}",
                    stringify!($p),
                    actual,
                )
            })
        }};
    }

    macro_rules! assert_error {
        ($e:expr, $p:pat) => {
            assert!(
                matches!($e, Err($p)),
                "Expected {}, but got {:?}",
                stringify!($p),
                $e,
            )
        };
    }

    impl<Actual> Assertion<Actual> for FailParsingWithAssertion
    where
        Actual: TryIntoTokens<Error = Error>,
    {
        type Error = BoxError;

        #[track_caller]
        fn assert(self, actual: Actual) -> Result<(), Self::Error> {
            self.0(actual.try_into_tokens());
            Ok(())
        }
    }

    #[test]
    fn xml_declaration() -> Result {
        expect(r#"<?xml version="1.0"?>"#).to(be_parsed_as([
            DeclarationStart(Complete("1.0")),
            DeclarationClose,
        ]))
    }

    #[test]
    fn xml_declaration_small_capacity() -> Result {
        expect(r#"<?xml version="1.123456789"?>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                DeclarationStart(Partial("1")),
                DeclarationStart(Complete(".123456789")),
                DeclarationClose,
            ]))
    }

    #[test]
    fn xml_declaration_single_quoted() -> Result {
        expect(r#"<?xml version='1.0'?>"#).to(be_parsed_as([
            DeclarationStart(Complete("1.0")),
            DeclarationClose,
        ]))
    }

    #[test]
    fn xml_declaration_encoding() -> Result {
        expect(r#"<?xml version="1.0" encoding="encoding"?>"#).to(be_parsed_as([
            DeclarationStart(Complete("1.0")),
            DeclarationEncoding(Complete("encoding")),
            DeclarationClose,
        ]))
    }

    #[test]
    fn xml_declaration_standalone() -> Result {
        expect(r#"<?xml version="1.0" standalone="yes"?>"#).to(be_parsed_as([
            DeclarationStart(Complete("1.0")),
            DeclarationStandalone(Complete("yes")),
            DeclarationClose,
        ]))
    }

    #[test]
    fn xml_declaration_encoding_and_standalone() -> Result {
        expect(r#"<?xml version="1.0" encoding="UCS-2" standalone="yes"?>"#).to(be_parsed_as([
            DeclarationStart(Complete("1.0")),
            DeclarationEncoding(Complete("UCS-2")),
            DeclarationStandalone(Complete("yes")),
            DeclarationClose,
        ]))
    }

    #[test]
    fn xml_declaration_with_spaces_around_attributes() -> Result {
        expect(r#"<?xml version = "1.0" encoding = "UCS-2" standalone = "yes" ?>"#).to(
            be_parsed_as([
                DeclarationStart(Complete("1.0")),
                DeclarationEncoding(Complete("UCS-2")),
                DeclarationStandalone(Complete("yes")),
                DeclarationClose,
            ]),
        )
    }

    #[test]
    fn self_closed_element() -> Result {
        expect(r#"<alpha />"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementSelfClose,
        ]))
    }

    #[test]
    fn self_closed_element_small_capacity() -> Result {
        expect(r#"<a01234567890123456789 />"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Partial("a01234567890123")),
                ElementOpenStart(Complete("456789")),
                ElementSelfClose,
            ]))
    }

    #[test]
    fn self_closed_element_with_one_attribute() -> Result {
        expect(r#"<alpha a="b"/>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            AttributeStart(Complete("a")),
            AttributeValueLiteral(Complete("b")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn self_closed_element_with_one_attribute_small_capacity() -> Result {
        expect(r#"<a01234567890123456789 b01234567890123456789="c01234567890123456789"/>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Partial("a01234567890123")),
                ElementOpenStart(Complete("456789")),
                AttributeStart(Partial("b01234567")),
                AttributeStart(Complete("890123456789")),
                AttributeValueLiteral(Partial("c012345678901234")),
                AttributeValueLiteral(Complete("56789")),
                AttributeValueEnd,
                ElementSelfClose,
            ]))
    }

    #[test]
    fn attributes_with_both_quote_styles() -> Result {
        expect(r#"<alpha a="b" c='d'/>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            AttributeStart(Complete("a")),
            AttributeValueLiteral(Complete("b")),
            AttributeValueEnd,
            AttributeStart(Complete("c")),
            AttributeValueLiteral(Complete("d")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn attribute_with_escaped_less_than_and_ampersand() -> Result {
        expect("<a b='&lt;&amp;' />").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            AttributeStart(Complete("b")),
            AttributeValueReferenceNamed(Complete("lt")),
            AttributeValueReferenceNamed(Complete("amp")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn attribute_with_references() -> Result {
        expect("<a b='&ten;&#10;&#x10;' />").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            AttributeStart(Complete("b")),
            AttributeValueReferenceNamed(Complete("ten")),
            AttributeValueReferenceDecimal(Complete("10")),
            AttributeValueReferenceHex(Complete("10")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn element_with_no_children() -> Result {
        expect(r#"<alpha></alpha>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementOpenEnd,
            ElementClose(Complete("alpha")),
        ]))
    }

    #[test]
    fn element_with_no_children_small_capacity() -> Result {
        expect(r#"<a01234567890123456789></a01234567890123456789>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Partial("a01234567890123")),
                ElementOpenStart(Complete("456789")),
                ElementOpenEnd,
                ElementClose(Partial("a012345")),
                ElementClose(Complete("67890123456789")),
            ]))
    }

    #[test]
    fn element_with_one_child() -> Result {
        expect(r#"<alpha><beta /></alpha>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementOpenEnd,
            ElementOpenStart(Complete("beta")),
            ElementSelfClose,
            ElementClose(Complete("alpha")),
        ]))
    }

    #[test]
    fn char_data() -> Result {
        expect("<a>b</a>").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            ElementOpenEnd,
            CharData(Complete("b")),
            ElementClose(Complete("a")),
        ]))
    }

    #[test]
    fn char_data_small_capacity() -> Result {
        expect(r#"<a>01234567890123456789</a>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
                CharData(Partial("0123456789012")),
                CharData(Complete("3456789")),
                ElementClose(Complete("a")),
            ]))
    }

    #[test]
    fn char_data_with_close_square_bracket() -> Result {
        expect("<a>b]</a>").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            ElementOpenEnd,
            CharData(Complete("b]")),
            ElementClose(Complete("a")),
        ]))
    }

    #[test]
    fn cdata() -> Result {
        expect("<![CDATA[ hello ]]>").to(be_parsed_as([CData(Complete(" hello "))]))
    }

    #[test]
    fn cdata_small_buffer() -> Result {
        expect("<![CDATA[aaaaaaaaaaaaaaaaaaaa]]>")
            .with(minimum_capacity)
            .to(be_parsed_as([
                CData(Partial("aaaaaaa")),
                CData(Complete("aaaaaaaaaaaaa")),
            ]))
    }

    #[test]
    fn only_space() -> Result {
        expect(" \t\r\n").to(be_parsed_as([CharData(Partial(" \t\r\n"))]))
    }

    #[test]
    fn leading_and_trailing_whitespace_self_closed() -> Result {
        expect("\t <a/>\r\n").to(be_parsed_as([
            CharData(Complete("\t ")),
            ElementOpenStart(Complete("a")),
            ElementSelfClose,
            CharData(Partial("\r\n")),
        ]))
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        expect("\t <a></a>\r\n").to(be_parsed_as([
            CharData(Complete("\t ")),
            ElementOpenStart(Complete("a")),
            ElementOpenEnd,
            ElementClose(Complete("a")),
            CharData(Partial("\r\n")),
        ]))
    }

    #[test]
    fn processing_instruction() -> Result {
        expect("<?a?>").to(be_parsed_as([
            ProcessingInstructionStart(Complete("a")),
            ProcessingInstructionEnd,
        ]))
    }

    #[test]
    fn processing_instruction_small_capacity() -> Result {
        expect("<?aaaaaaaaaaaaaaaaaaaa?>")
            .with(minimum_capacity)
            .to(be_parsed_as([
                ProcessingInstructionStart(Partial("aaaaaaaaaaaaaa")),
                ProcessingInstructionStart(Complete("aaaaaa")),
                ProcessingInstructionEnd,
            ]))
    }

    #[test]
    fn processing_instruction_starts_with_xml() -> Result {
        expect("<?xml-but-not-that?>").to(be_parsed_as([
            ProcessingInstructionStart(Complete("xml-but-not-that")),
            ProcessingInstructionEnd,
        ]))
    }

    #[test]
    fn processing_instruction_with_value() -> Result {
        expect("<?a b?>").to(be_parsed_as([
            ProcessingInstructionStart(Complete("a")),
            ProcessingInstructionValue(Complete("b")),
            ProcessingInstructionEnd,
        ]))
    }

    #[test]
    fn processing_instruction_with_value_small_buffer() -> Result {
        expect("<?aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbb?>")
            .with(minimum_capacity)
            .to(be_parsed_as([
                ProcessingInstructionStart(Partial("aaaaaaaaaaaaaa")),
                ProcessingInstructionStart(Complete("aaaaaa")),
                ProcessingInstructionValue(Partial("bbbbbbbbb")),
                ProcessingInstructionValue(Complete("bbbbbbbbbbb")),
                ProcessingInstructionEnd,
            ]))
    }

    #[test]
    fn comment() -> Result {
        expect("<!-- hello -->").to(be_parsed_as([Comment(Complete(" hello "))]))
    }

    #[test]
    fn comment_small_buffer() -> Result {
        expect("<!--aaaaaaaaaaaaaaaaaaaa-->")
            .with(minimum_capacity)
            .to(be_parsed_as([
                Comment(Partial("aaaaaaaaaaaa")),
                Comment(Complete("aaaaaaaa")),
            ]))
    }

    #[test]
    fn reference_named() -> Result {
        expect("&lt;").to(be_parsed_as([ReferenceNamed(Complete("lt"))]))
    }

    #[test]
    fn reference_named_small_buffer() -> Result {
        expect("&aaaaaaaaaaaaaaaaaaaa;")
            .with(minimum_capacity)
            .to(be_parsed_as([
                ReferenceNamed(Partial("aaaaaaaaaaaaaaa")),
                ReferenceNamed(Complete("aaaaa")),
            ]))
    }

    #[test]
    fn reference_decimal() -> Result {
        expect("&#42;").to(be_parsed_as([ReferenceDecimal(Complete("42"))]))
    }

    #[test]
    fn reference_decimal_small_buffer() -> Result {
        expect("&#11111111111111111111;")
            .with(minimum_capacity)
            .to(be_parsed_as([
                ReferenceDecimal(Partial("11111111111111")),
                ReferenceDecimal(Complete("111111")),
            ]))
    }

    #[test]
    fn reference_hex() -> Result {
        expect("&#xBEEF;").to(be_parsed_as([ReferenceHex(Complete("BEEF"))]))
    }

    #[test]
    fn reference_hex_small_buffer() -> Result {
        expect("&#xaaaaaaaaaaaaaaaaaaaa;")
            .with(minimum_capacity)
            .to(be_parsed_as([
                ReferenceHex(Partial("aaaaaaaaaaaaa")),
                ReferenceHex(Complete("aaaaaaa")),
            ]))
    }

    // After parsing a name to the end of the buffer, when we start
    // parsing again, we need to allow the first character to be a
    // non-start-char.
    #[test]
    fn names_that_span_blocks_can_continue_with_non_start_chars() -> Result {
        expect(r#"<a----------------/>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Partial("a--------------")),
                ElementOpenStart(Complete("--")),
                ElementSelfClose,
            ]))
    }

    #[test]
    fn multi_byte_lookahead_at_end_of_input() -> Result {
        // Parser looked for `<?`, which didn't fit and since
        // we ran out of input, we got an error.
        let input = " ";

        expect(input)
            .with(minimum_capacity)
            .to(be_parsed_as([CharData(Partial(" "))]))
    }

    #[test]
    fn multi_byte_processing_instruction_lookahead_that_spans_blocks() -> Result {
        // Parser looked for `?>`, which was split across the current
        // buffer and the next.
        let input = "<?a aaaaaaaaaaaaaaaaaaaaaaaaaaa?>";

        expect(input).with(capacity(32)).to(be_parsed_as([
            ProcessingInstructionStart(Complete("a")),
            ProcessingInstructionValue(Partial("aaaaaaaaaaaaaaaaaaaaaaaaaaa")),
            ProcessingInstructionValue(Complete("")),
            ProcessingInstructionEnd,
        ]))
    }

    #[test]
    fn multi_byte_comment_lookahead_that_spans_blocks_1() -> Result {
        let input = "<!--aaaaaaaaaaaaaaaaaaaaaaaaaaa-->";
        //                         The last byte is ^
        expect(input).with(capacity(32)).to(be_parsed_as([
            Comment(Partial("aaaaaaaaaaaaaaaaaaaaaaaaaaa")),
            Comment(Complete("")),
        ]))
    }

    #[test]
    fn multi_byte_cdata_lookahead_that_spans_blocks_1() -> Result {
        let input = "<![CDATA[aaaaaaaaaaaaaaaaaaaaaa]]>";
        //      this is the last byte in the buffer ^
        expect(input).with(capacity(32)).to(be_parsed_as([
            CData(Partial("aaaaaaaaaaaaaaaaaaaaaa")),
            CData(Complete("")),
        ]))
    }

    #[test]
    fn multi_byte_cdata_lookahead_that_spans_blocks_2() -> Result {
        let input = "<![CDATA[aaaaaaaaaaaaaaaaaaaaa]]>";
        //      this is the last byte in the buffer ^
        expect(input).with(capacity(32)).to(be_parsed_as([
            CData(Partial("aaaaaaaaaaaaaaaaaaaaa")),
            CData(Complete("")),
        ]))
    }

    /// These tests all focus on locations where arbitrary-length
    /// space tokens might fill the entire buffer. Each of these needs
    /// to result in a separate state so that we can properly resume
    /// after the user refills the buffer.
    mod space_fills_rest_of_buffer {
        use super::*;

        #[test]
        fn after_declaration_open() -> Result {
            for i in 1..=64 {
                let input = format!("<?xml{}version='1.0' ?>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?
            }

            let input = "<?xml                  version='1.0' ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3

            expect(&*input).with(capacity(32)).to(be_parsed_as([
                DeclarationStart(Complete("1.0")),
                DeclarationClose,
            ]))
        }

        #[test]
        fn after_declaration_version_into_encoding() -> Result {
            for i in 1..=64 {
                let input = format!("<?xml version='1.0'{}encoding='UTF-8' ?>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<?xml version='1.0'     encoding='UTF-8' ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3

            expect(&*input).with(capacity(32)).to(be_parsed_as([
                DeclarationStart(Complete("1.0")),
                DeclarationEncoding(Complete("UTF-8")),
                DeclarationClose,
            ]))
        }

        #[test]
        fn after_declaration_standalone_into_standalone() -> Result {
            for i in 1..=64 {
                let input = format!("<?xml version='1.0'{}standalone='no' ?>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<?xml version='1.0'   standalone='no' ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3

            expect(&*input).with(capacity(32)).to(be_parsed_as([
                DeclarationStart(Complete("1.0")),
                DeclarationStandalone(Complete("no")),
                DeclarationClose,
            ]))
        }

        #[test]
        fn after_open_element_name() -> Result {
            for i in 1..=64 {
                let input = format!("<a{}>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<a                                                             >";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            expect(&*input).with(capacity(32)).to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
            ]))
        }

        #[test]
        fn after_attribute_name() -> Result {
            for i in 1..=64 {
                let input = format!("<a b{}='c' />", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<a b                           ='c' />";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            expect(input).with(capacity(32)).to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                AttributeStart(Complete("b")),
                AttributeValueLiteral(Complete("c")),
                AttributeValueEnd,
                ElementSelfClose,
            ]))
        }

        #[test]
        fn after_attribute_equal() -> Result {
            for i in 1..=64 {
                let input = format!("<a b={}'c' />", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<a b=                              'c' />";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            expect(input).with(capacity(32)).to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                AttributeStart(Complete("b")),
                AttributeValueLiteral(Complete("c")),
                AttributeValueEnd,
                ElementSelfClose,
            ]))
        }

        #[test]
        fn after_close_element() -> Result {
            for i in 1..=64 {
                let input = format!("</a{}>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "</a                               >";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            expect(input)
                .with(capacity(32))
                .to(be_parsed_as([ElementClose(Complete("a"))]))
        }

        #[test]
        fn after_processing_instruction_name() -> Result {
            for i in 1..=64 {
                let input = format!("<?a{}?>", " ".repeat(i));
                expect(&*input).with(capacity(32)).to(parse)?;
            }

            let input = "<?a                               ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            expect(input).with(capacity(32)).to(be_parsed_as([
                ProcessingInstructionStart(Complete("a")),
                ProcessingInstructionEnd,
            ]))
        }
    }

    #[test]
    fn fail_declaration_unclosed() -> Result {
        expect(r"<?xml").to(fail_parsing_with!(Error::IncompleteXml))
    }

    #[test]
    fn fail_declaration_missing_version() -> Result {
        expect(r"<?xml?>").to(fail_parsing_with!(Error::RequiredTokenMissing {
            token: RequiredToken::Version,
            location: 5
        }))
    }

    #[test]
    fn fail_element_missing_space() -> Result {
        expect(r"<a\b='c'>").to(fail_parsing_with!(Error::RequiredSpaceMissing {
            location: 2
        }))
    }

    #[test]
    fn fail_attribute_with_unescaped_less_than() -> Result {
        expect("<a b='<'").to(fail_parsing_with!(Error::InvalidCharacterInAttribute {
            location: 6
        }))
    }

    #[test]
    fn fail_attribute_with_unescaped_ampersand() -> Result {
        expect("<a b='&'").to(fail_parsing_with!(Error::RequiredTokenMissing {
            token: RequiredToken::Semicolon,
            location: 7
        }))
    }

    #[test]
    fn fail_processing_instruction_missing_space() -> Result {
        expect(r"<?a\b?>").to(fail_parsing_with!(Error::RequiredSpaceMissing {
            location: 3
        }))
    }

    #[test]
    fn fail_comments_disallow_double_hyphens() -> Result {
        expect("<!------>").to(fail_parsing_with!(Error::DoubleHyphenInComment {
            location: 4
        }))
    }

    #[test]
    fn fail_comment_unclosed() -> Result {
        expect(r##"<!--hello"##).to(fail_parsing_with!(Error::IncompleteXml))
    }

    #[test]
    fn fail_processing_instruction_unclosed() -> Result {
        expect(r"<?a").to(fail_parsing_with!(Error::IncompleteXml))
    }

    #[test]
    fn fail_disallowed_ascii_char() -> Result {
        let mut x = [b'a'; 20];
        x[19] = 0x07;

        expect(&x[..])
            .with(capacity(16))
            .to(fail_parsing_with!(Error::InvalidChar {
                location: 19,
                length: 1
            }))
    }

    #[test]
    fn fail_disallowed_multibyte_char() -> Result {
        let mut x = [b'a'; 20];
        x[17] = 0xEF;
        x[18] = 0xBF;
        x[19] = 0xBF;

        expect(&x[..])
            .with(capacity(16))
            .to(fail_parsing_with!(Error::InvalidChar {
                location: 17,
                length: 3
            }))
    }

    #[test]
    fn fail_non_utf_8() -> Result {
        expect(&[b'a', b'b', b'c', 0xFF][..]).to(fail_parsing_with!(Error::InputNotUtf8 {
            location: 3,
            length: 1,
        }))
    }

    #[test]
    fn ensure_state_is_small() {
        assert_eq!(2, std::mem::size_of::<State>());
    }

    mod fuse {
        use super::*;

        #[test]
        fn combines_split_tokens() -> Result {
            let tokens = FuseCore::fuse_all(vec![
                ElementOpenStart(Partial("aaaaa")),
                ElementOpenStart(Complete("aaaaa")),
                AttributeStart(Partial("")),
                AttributeStart(Partial("bbbbbbbbbb")),
                AttributeStart(Complete("")),
                AttributeValueLiteral(Partial("c")),
                AttributeValueLiteral(Partial("c")),
                AttributeValueLiteral(Complete("c")),
                AttributeValueEnd,
                ElementSelfClose,
            ])?;

            assert_eq!(
                tokens,
                [
                    FusedToken::ElementOpenStart("aaaaaaaaaa"),
                    AttributeStart("bbbbbbbbbb"),
                    AttributeValueLiteral(Partial("c")),
                    AttributeValueLiteral(Partial("c")),
                    AttributeValueLiteral(Complete("c")),
                    AttributeValueEnd,
                    ElementSelfClose,
                ],
            );

            Ok(())
        }

        #[test]
        fn unfinished_character_data_is_returned() -> Result {
            let tokens = FuseCore::fuse_all(vec![CharData(Partial("a"))])?;

            assert_eq!(tokens, [FusedToken::CharData(Partial("a"))]);

            Ok(())
        }

        #[test]
        fn fail_unfinished_tokens() -> Result {
            let error = FuseCore::fuse_all(vec![ElementOpenStart(Partial("a"))]);

            assert_error!(error, FuseError::Incomplete);

            Ok(())
        }
    }

    impl<R> Parser<R>
    where
        R: Read,
    {
        fn collect_owned(&mut self) -> super::Result<OwnedTokens> {
            let mut v = vec![];
            while let Some(t) = self.next() {
                v.push(t?.map(|s| s.map(str::to_owned)));
            }
            Ok(v)
        }
    }

    #[derive(Debug, Copy, Clone)]
    struct FusedOwnedKind(());

    macro_rules! fused_owned_kind {
        ($($tt:tt $name:ident,)*) => { $(fused_owned_kind! { @type $tt $name })* };

        (@type pass $name:ident) => { };
        (@type fuse $name:ident) => { type $name = String; };
        (@type stream $name:ident) => { type $name = Streaming<String>; };
    }

    impl TokenKind for FusedOwnedKind {
        fuse_invoke!(fused_owned_kind);
    }

    type FusedOwnedToken = Token<FusedOwnedKind>;

    #[ext]
    impl FusedOwnedToken {
        fn from_index(token: FusedIndexToken, buffer: &str, source: &impl Exchange) -> Self {
            use FusedIndex::*;

            macro_rules! fuse_all_match {
                ($($tt:tt $name:ident,)*) => {
                    match token {
                        $( fuse_all_match!(@pat $tt $name s) => fuse_all_match!(@arm $tt $name s), )*
                    }
                };

                (@pat pass $name:ident $s:ident) => { $name };
                (@arm pass $name:ident $s:ident) => { $name };

                (@pat fuse $name:ident $s:ident) => { $name($s) };
                (@arm fuse $name:ident $s:ident) => {
                    match $s {
                        Buffered => $name(buffer.to_string()),
                        Direct(idx) => $name(source.exchange(idx).to_string()),
                    }
                };

                (@pat stream $name:ident $s:ident) => { $name($s) };
                (@arm stream $name:ident $s:ident) => { $name($s.map(|i| source.exchange(i).to_string())) };
            }

            fuse_invoke!(fuse_all_match)
        }
    }

    struct BufferedParser<'a>(Vec<&'a str>);

    impl<'a> BufferedParser<'a> {
        fn new(
            tokens: impl IntoIterator<Item = UniformToken<Streaming<&'a str>>>,
        ) -> (Self, Vec<IndexToken>) {
            let mut buffered = vec![];
            let mut index_tokens = vec![];

            for (i, t) in tokens.into_iter().enumerate() {
                let t = t.map(|s| {
                    s.map(|v| {
                        buffered.push(v);
                        i
                    })
                });
                index_tokens.push(t);
            }

            (BufferedParser(buffered), index_tokens)
        }
    }

    impl Exchange for BufferedParser<'_> {
        fn exchange(&self, idx: usize) -> &str {
            self.0[idx]
        }
    }

    impl FuseCore {
        fn fuse_all<'a>(
            tokens: impl IntoIterator<Item = UniformToken<Streaming<&'a str>>>,
        ) -> super::Result<Vec<FusedOwnedToken>, super::FuseError> {
            let (parser, index_tokens) = BufferedParser::new(tokens);

            let mut collected = vec![];
            let mut me = Self::default();

            for token in index_tokens {
                collected.extend({
                    let idx = me.push(token, &parser);
                    idx.map(|i| FusedOwnedToken::from_index(i, &me.buffer, &parser))
                });
            }

            collected.extend({
                let idx = me.finish()?;
                idx.map(|i| FusedOwnedToken::from_index(i, &me.buffer, &parser))
            });

            Ok(collected)
        }
    }
}
