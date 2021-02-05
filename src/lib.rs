#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use async_trait::async_trait;
use easy_ext::ext;
use itertools::Itertools;
use snafu::{ensure, Snafu};
use std::{mem, str};

#[async_trait(?Send)]
pub trait DataSource {
    async fn read(&mut self, buffer: &mut [u8]) -> usize;
}

/// Adapts an implementer of [`std::io::Read`].
///
/// This should **not** be used unless you are also using the parser
/// from a completely synchronous context.
#[derive(Debug)]
pub struct ReadAdapter<R>(R);

impl<R> ReadAdapter<R>
where
    R: std::io::Read,
{
    pub fn new(read: R) -> Self {
        Self(read)
    }
}

#[async_trait(?Send)]
impl<R> DataSource for ReadAdapter<R>
where
    R: std::io::Read,
{
    async fn read(&mut self, buffer: &mut [u8]) -> usize {
        self.0.read(buffer).expect("TODO")
    }
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
        // TODO: Alternate implementation without checks
        let bytes = &self.buffer[self.n_offset_bytes..][..self.n_utf8_bytes];
        str::from_utf8(bytes).expect("Safety invariant failed")
    }

    async fn some_str(&mut self) -> Result<&str> {
        if self.n_utf8_bytes == 0 {
            self.extend().await?;
        }
        Ok(self.as_str())
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
            self.extend().await?;
        }

        Ok(self.as_str().starts_with(needle))
    }

    fn advance(&mut self, n_bytes: usize) {
        self.n_offset_bytes += n_bytes;
        self.n_utf8_bytes = self.n_utf8_bytes.saturating_sub(n_bytes);
    }

    async fn consume(&mut self, s: &str) -> Result<MustUse<bool>> {
        if self.starts_with(s).await? {
            self.advance(s.len());
            Ok(MustUse(true))
        } else {
            Ok(MustUse(false))
        }
    }

    async fn require(&mut self, s: &str) -> Result<()> {
        ensure!(
            *self.consume(s).await?,
            RequiredTokenMissing {
                token: s,
                location: self.absolute_location(),
            }
        );
        Ok(())
    }

    async fn consume_until(&mut self, needle: &str) -> Result<Streaming<&str>> {
        let mut s = self.as_str();
        if s.is_empty() {
            self.extend().await?;
            s = self.as_str();
        }

        match s.find(needle) {
            Some(x) => Ok(Streaming::Complete(&self.as_str()[..x])),
            None => Ok(Streaming::Partial(self.as_str())),
        }
    }

    async fn space(&mut self) -> Result<Option<usize>> {
        let s = self.some_str().await?;

        let space = s
            .char_indices()
            .peeking_take_while(|(_, c)| c.is_space())
            .last()
            .map(|(i, c)| i + c.len_utf8());

        match space {
            Some(0) => Ok(None),
            Some(space) => Ok(Some(space)),
            None => Ok(None),
        }
    }

    async fn consume_space(&mut self) -> Result<()> {
        while let Some(len) = self.space().await? {
            self.advance(len);
        }

        Ok(())
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
            .map(|(i, c)| i + c.len_utf8())
            .last()
            .unwrap_or(end_idx);

        if end_idx == s.len() {
            Ok(Streaming::Partial(s))
        } else {
            Ok(Streaming::Complete(&s[..end_idx]))
        }
    }

    async fn name_continuation(&mut self) -> Result<Streaming<&str>> {
        let s = self.some_str().await?;

        let c = s.char_indices();

        let end_idx = match c
            .take_while(|(_, c)| c.is_name_char())
            .map(|(i, c)| i + c.len_utf8())
            .last()
        {
            Some(i) => i,
            None => return Ok(Streaming::Complete("")),
        };

        if end_idx == s.len() {
            Ok(Streaming::Partial(s))
        } else {
            Ok(Streaming::Complete(&s[..end_idx]))
        }
    }
}

#[ext]
impl char {
    fn is_space(self) -> bool {
        match self {
            '\u{20}' | '\u{9}' | '\u{D}' | '\u{A}' => true,
            _ => false,
        }
    }

    fn is_name_start_char(self) -> bool {
        match self {
            ':'
            | 'A'..='Z'
            | '_'
            | 'a'..='z'
            | '\u{C0}'..='\u{D6}'
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

    fn is_name_char(self) -> bool {
        if self.is_name_start_char() {
            return true;
        }

        match self {
            '-'
            | '.'
            | '0'..='9'
            | '\u{B7}'
            | '\u{0300}'..='\u{036F}'
            | '\u{203F}'..='\u{2040}' => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    Initial,

    StreamElementOpenName,
    AfterElementOpenName,

    StreamAttributeName,
    AfterAttributeName,
    StreamAttributeValue,

    StreamElementCloseName,
    AfterElementCloseName,
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

    fn unify(&self) -> &T {
        match self {
            Self::Partial(a) => a,
            Self::Complete(a) => a,
        }
    }
}

impl<T> Streaming<&T>
where
    T: ToOwned + ?Sized,
{
    fn into_owned(self) -> Streaming<T::Owned> {
        match self {
            Streaming::Partial(v) => Streaming::Partial(v.to_owned()),
            Streaming::Complete(v) => Streaming::Complete(v.to_owned()),
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
pub enum Token<'a> {
    /// `<foo`
    ElementOpenStart(Streaming<&'a str>),
    /// `>`
    ElementOpenEnd,
    /// `/>`
    ElementSelfClose,

    /// `</foo`
    ElementClose(Streaming<&'a str>),

    /// `foo`
    AttributeName(Streaming<&'a str>),
    /// `="bar`
    AttributeValue(Streaming<&'a str>),

    Space(Streaming<&'a str>),
}

#[async_trait(?Send)]
pub trait TokenSource {
    async fn next(&mut self) -> Option<Result<Token<'_>>>;
}

// TODO: Should we replace this with generics? We always know exactly
// what to do at compile time.
#[derive(Debug)]
enum NameKind {
    Start,
    Continue,
}

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

    fn with_buffer_capacity(source: S, capacity: usize) -> Self {
        Parser {
            buffer: StringRing::with_buffer_capacity(source, capacity),
            state: State::Initial,
            to_advance: 0,
        }
    }

    async fn dispatch_initial(&mut self) -> Result<Option<Token<'_>>> {
        use {State::*, Token::*};

        if self.buffer.complete().await? {
            Ok(None)
        } else if let Some(l) = self.buffer.space().await? {
            self.to_advance = l;
            Ok(Some(Space(Streaming::Complete(&self.buffer.as_str()[..l]))))
        } else if *self.buffer.consume("</").await? {
            self.state = StreamElementCloseName;
            self.dispatch_stream_element_close_name(NameKind::Start)
                .await
        } else if *self.buffer.consume("<").await? {
            self.state = StreamElementOpenName;
            self.dispatch_stream_element_open_name(NameKind::Start)
                .await
        } else {
            unimplemented!()
        }
    }

    async fn dispatch_stream_element_open_name(
        &mut self,
        name_kind: NameKind,
    ) -> Result<Option<Token<'_>>> {
        self.stream_name(
            name_kind,
            State::AfterElementOpenName,
            Token::ElementOpenStart,
        )
        .await
    }

    async fn dispatch_stream_element_close_name(
        &mut self,
        name_kind: NameKind,
    ) -> Result<Option<Token<'_>>> {
        self.stream_name(name_kind, State::AfterElementCloseName, Token::ElementClose)
            .await
    }

    async fn dispatch_after_element_open_name(&mut self) -> Result<Option<Token<'_>>> {
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
            self.dispatch_stream_attribute_name(NameKind::Start).await
        }
    }

    async fn dispatch_stream_attribute_name(
        &mut self,
        name_kind: NameKind,
    ) -> Result<Option<Token<'_>>> {
        self.stream_name(name_kind, State::AfterAttributeName, Token::AttributeName)
            .await
    }

    async fn dispatch_after_attribute_name(&mut self) -> Result<Option<Token<'_>>> {
        use State::*;

        self.buffer.consume_space().await?;
        self.buffer.require("=").await?;
        self.buffer.consume_space().await?;

        self.buffer.require("\"").await?;

        self.state = StreamAttributeValue;
        self.dispatch_stream_attribute_value().await
    }

    async fn dispatch_stream_attribute_value(&mut self) -> Result<Option<Token<'_>>> {
        use {State::*, Token::*};

        let value = self.buffer.consume_until("\"").await?;

        self.to_advance = value.unify().len();

        if value.is_complete() {
            self.state = AfterElementOpenName;
            self.to_advance += 1 // Include the closing quote
        }

        Ok(Some(AttributeValue(value)))
    }

    async fn dispatch_after_element_close_name(&mut self) -> Result<Option<Token<'_>>> {
        self.buffer.consume_space().await?;
        self.buffer.require(">").await?;

        self.dispatch_initial().await
    }

    // ----------

    async fn stream_name<'a>(
        &'a mut self,
        name_kind: NameKind,
        next_state: State,
        create: impl FnOnce(Streaming<&'a str>) -> Token<'a>,
    ) -> Result<Option<Token<'_>>> {
        let name = match name_kind {
            NameKind::Start => self.buffer.name().await?,
            NameKind::Continue => self.buffer.name_continuation().await?,
        };

        self.to_advance = name.unify().len();

        if name.is_complete() {
            self.state = next_state;
        }
        Ok(Some(create(name)))
    }
}

#[async_trait(?Send)]
impl<S> TokenSource for Parser<S>
where
    S: DataSource,
{
    async fn next(&mut self) -> Option<Result<Token<'_>>> {
        use State::*;

        let to_advance = mem::take(&mut self.to_advance);
        self.buffer.advance(to_advance);

        match self.state {
            Initial => self.dispatch_initial().await,

            StreamElementOpenName => {
                self.dispatch_stream_element_open_name(NameKind::Continue)
                    .await
            }
            AfterElementOpenName => self.dispatch_after_element_open_name().await,

            StreamAttributeName => {
                self.dispatch_stream_attribute_name(NameKind::Continue)
                    .await
            }
            AfterAttributeName => self.dispatch_after_attribute_name().await,
            StreamAttributeValue => self.dispatch_stream_attribute_value().await,

            StreamElementCloseName => {
                self.dispatch_stream_element_close_name(NameKind::Continue)
                    .await
            }
            AfterElementCloseName => self.dispatch_after_element_close_name().await,
        }
        .transpose()
    }
}

// https://github.com/rust-lang/rust/issues/78149
#[must_use]
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
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[cfg(test)]
mod test {
    use easy_ext::ext;
    use futures::executor::block_on;

    use super::*;

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

    macro_rules! assert_error {
        ($e:expr, $p:pat) => {
            assert!(
                matches!($e, Err($p)),
                "Expected {}, but got {:?}",
                stringify!($p),
                $e
            )
        };
    }

    #[test]
    fn self_closed_element() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str(r#"<alpha />"#).collect_owned().await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [ElementOpenStart(Complete("alpha")), ElementSelfClose]
            );

            Ok(())
        })
    }

    #[test]
    fn self_closed_element_small_capacity() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str_and_min_capacity(r#"<a01234567890123456789 />"#)
                .collect_owned()
                .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Partial("a01234567890123")),
                    ElementOpenStart(Complete("456789")),
                    ElementSelfClose,
                ]
            );

            Ok(())
        })
    }

    #[test]
    fn self_closed_element_with_one_attribute() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str(r#"<alpha a="b"/>"#)
                .collect_owned()
                .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Complete("alpha")),
                    AttributeName(Complete("a")),
                    AttributeValue(Complete("b")),
                    ElementSelfClose,
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn self_closed_element_with_one_attribute_small_capacity() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str_and_min_capacity(
                r#"<a01234567890123456789 b01234567890123456789="c01234567890123456789"/>"#,
            )
            .collect_owned()
            .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Partial("a01234567890123")),
                    ElementOpenStart(Complete("456789")),
                    AttributeName(Partial("b01234567")),
                    AttributeName(Complete("890123456789")),
                    AttributeValue(Partial("c0")),
                    AttributeValue(Partial("1234567890123456")),
                    AttributeValue(Complete("789")),
                    ElementSelfClose,
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn element_with_no_children() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str(r#"<alpha></alpha>"#)
                .collect_owned()
                .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Complete("alpha")),
                    ElementOpenEnd,
                    ElementClose(Complete("alpha")),
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn element_with_no_children_small_capacity() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str_and_min_capacity(
                r#"<a01234567890123456789></a01234567890123456789>"#,
            )
            .collect_owned()
            .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Partial("a01234567890123")),
                    ElementOpenStart(Complete("456789")),
                    ElementOpenEnd,
                    ElementClose(Partial("a012345")),
                    ElementClose(Complete("67890123456789")),
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn element_with_one_child() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str(r#"<alpha><beta /></alpha>"#)
                .collect_owned()
                .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Complete("alpha")),
                    ElementOpenEnd,
                    ElementOpenStart(Complete("beta")),
                    ElementSelfClose,
                    ElementClose(Complete("alpha")),
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn only_space() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str(" \t\r\n").collect_owned().await?;

            use {Streaming::*, Token::*};
            assert_eq!(tokens, [Space(Complete(" \t\r\n"))]);

            Ok(())
        })
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str("\t <a/>\r\n").collect_owned().await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    Space(Complete("\t ")),
                    ElementOpenStart(Complete("a")),
                    ElementSelfClose,
                    Space(Complete("\r\n")),
                ],
            );

            Ok(())
        })
    }

    // After parsing a name to the end of the buffer, when we start
    // parsing again, we need to allow the first character to be a
    // non-start-char.
    #[test]
    fn names_that_span_blocks_can_continue_with_non_start_chars() -> Result {
        block_on(async {
            let tokens = Parser::new_from_str_and_min_capacity(r#"<a----------------/>"#)
                .collect_owned()
                .await?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Partial("a--------------")),
                    ElementOpenStart(Complete("--")),
                    ElementSelfClose,
                ],
            );

            Ok(())
        })
    }

    #[test]
    fn fail_one_character() -> Result {
        block_on(async {
            let error = Parser::new_from_str(r#"a"#).collect_owned().await;

            assert_error!(error, Error::NoMoreInputAvailable);

            Ok(())
        })
    }

    #[test]
    fn fail_non_utf_8() -> Result {
        block_on(async {
            let error = Parser::new_from_bytes(&[b'a', b'b', b'c', 0xFF])
                .collect_owned()
                .await;

            assert_error!(
                error,
                Error::InputNotUtf8 {
                    location: 3,
                    length: 1,
                }
            );

            Ok(())
        })
    }

    impl<'a> Parser<ReadAdapter<&'a [u8]>> {
        fn new_from_str(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::DEFAULT_CAPACITY)
        }

        fn new_from_str_and_min_capacity(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::MINIMUM_CAPACITY)
        }

        fn new_from_str_and_capacity(s: &'a str, capacity: usize) -> Self {
            let src = ReadAdapter::new(s.as_bytes());
            Parser::with_buffer_capacity(src, capacity)
        }

        fn new_from_bytes(s: &'a [u8]) -> Self {
            Self::new_from_bytes_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::DEFAULT_CAPACITY)
        }

        fn new_from_bytes_and_capacity(s: &'a [u8], capacity: usize) -> Self {
            let src = ReadAdapter::new(s);
            Parser::with_buffer_capacity(src, capacity)
        }
    }

    macro_rules! mirror_owned_token {
        (
            $(#[$meta:meta])*
            enum $e_name:ident {
                $($v_name:ident $(($field:ty))?,)*
            }
        ) => {

            $(#[$meta])*
            enum $e_name {
                $($v_name $(($field))? ,)*
            }

            impl From<Token<'_>> for $e_name {
                fn from(other: Token<'_>) -> Self {
                    match other {
                        $(mirror_owned_token!(@from_pat s $v_name $($field)?) =>
                          mirror_owned_token!(@from_arm s $v_name $($field)?) ,)*
                    }
                }
            }

            impl PartialEq<Token<'_>> for $e_name {
                fn eq(&self, other: &Token<'_>) -> bool {
                    match (self, other) {
                        $(mirror_owned_token!(@eq_pat s1 s2 $v_name $($field)?) =>
                          mirror_owned_token!(@eq_arm s1 s2 $v_name $($field)?) ,)*
                        _ => false,
                    }
                }
            }
        };

        (@from_pat $s:ident $v_name:ident $field:ty) => { Token::$v_name($s) };
        (@from_arm $s:ident $v_name:ident $field:ty) => { Self::$v_name($s.into_owned()) };
        (@from_pat $s:ident $v_name:ident) => { Token::$v_name };
        (@from_arm $s:ident $v_name:ident) => { Self::$v_name };

        (@eq_pat $s1:ident $s2:ident $v_name:ident $field:ty) => { (Self::$v_name($s1), Token::$v_name($s2)) };
        (@eq_arm $s1:ident $s2:ident $v_name:ident $field:ty) => { $s1 == $s2 };
        (@eq_pat $s1:ident $s2:ident $v_name:ident) => { (Self::$v_name, Token::$v_name) };
        (@eq_arm $s1:ident $s2:ident $v_name:ident) => { true };
    }

    mirror_owned_token! {
        /// Owns all the string data to avoid keeping references alive
        #[derive(Debug, Clone, PartialEq, Eq)]
        enum OwnedToken {
            ElementOpenStart(Streaming<String>),
            ElementOpenEnd,
            ElementSelfClose,

            ElementClose(Streaming<String>),

            AttributeName(Streaming<String>),
            AttributeValue(Streaming<String>),

            Space(Streaming<String>),
        }
    }

    #[ext]
    #[async_trait(?Send)]
    impl<T> T
    where
        Self: TokenSource,
    {
        async fn collect_owned(&mut self) -> super::Result<Vec<OwnedToken>> {
            let mut v = vec![];
            while let Some(t) = self.next().await {
                v.push(t?.into());
            }
            Ok(v)
        }
    }
}
