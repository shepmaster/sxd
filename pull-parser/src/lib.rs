#![deny(rust_2018_idioms)]

use easy_ext::ext;
use memchr::memmem;
use snafu::{ensure, Snafu};
use std::{
    fmt,
    io::{self, Read},
    marker::PhantomData,
    ops, str,
};
use token::{IsComplete, Streaming, Token, TokenKind, UniformToken};
use xml_str::{SliceExt, U8Ext};

#[macro_use]
mod macros;

struct MaybeUtf8<'a>(&'a [u8]);

impl<'a> fmt::Debug for MaybeUtf8<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chunks = self.0.utf8_chunks();

        let one_chunk = |f: &mut fmt::Formatter<'_>, c: str::Utf8Chunk<'_>| {
            c.valid().fmt(f)?;
            write!(f, ", ")?;

            let mut invalid = c.invalid().iter();
            if let Some(i) = invalid.next() {
                write!(f, "0x{i:02x}")?;

                for i in invalid {
                    write!(f, ", ")?;
                    write!(f, "0x{i:02x}")?;
                }
            }

            fmt::Result::Ok(())
        };

        if let Some(c) = chunks.next() {
            one_chunk(f, c)?;
            for c in chunks {
                write!(f, ", ")?;
                one_chunk(f, c)?;
            }
        }

        Ok(())
    }
}

// TODO
/// Callers should always have this many bytes or else may never progress.
// The longest single token should be `standalone`, then round up a bit.
#[cfg(test)]
const MINIMUM_CAPACITY: usize = 16;
const DEFAULT_CAPACITY: usize = 1024;

struct BufAdvance<'b> {
    buffer: &'b [u8],
    advance: usize,
    checkpoint: usize,
    exhausted: bool,
}

impl fmt::Debug for BufAdvance<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("BufAdvance")
            .field("buffer", &MaybeUtf8(self.buffer))
            .field("advance", &self.advance)
            .field("checkpoint", &self.checkpoint)
            .field("exhausted", &self.exhausted)
            .finish()
    }
}

impl<'b> BufAdvance<'b> {
    fn new(buffer: &'b [u8], exhausted: bool) -> Self {
        Self {
            buffer,
            advance: 0,
            checkpoint: 0,
            exhausted,
        }
    }

    fn checkpoint(&mut self) {
        self.checkpoint = self.advance;
    }

    /// Tries to return a string with the given byte length, but if
    /// the input is exhausted, the returned string may be shorter.
    fn weak_min_str(&self, len: usize) -> Result<&'b [u8]> {
        ensure!(
            self.buffer.len() >= len || self.exhausted,
            NeedsMoreInputSnafu {
                advance: self.checkpoint
            }
        );

        Ok(self.buffer)
    }

    fn min_bytes(&self, len: usize) -> Result<&'b [u8]> {
        ensure!(
            self.buffer.enough_to_parse() && self.buffer.len() >= len,
            NeedsMoreInputSnafu {
                advance: self.checkpoint,
            }
        );

        Ok(self.buffer)
    }

    fn some_str(&self) -> Result<&'b [u8]> {
        ensure!(
            self.buffer.enough_to_parse(),
            NeedsMoreInputSnafu {
                advance: self.checkpoint,
            }
        );

        Ok(self.buffer)
    }

    fn absolute_location(&self) -> usize {
        self.advance
    }

    fn starts_with(&self, needle: &[u8]) -> Result<bool> {
        let s = abandon!(self.weak_min_str(needle.len()));
        Ok(s.starts_with(needle))
    }

    #[inline(always)]
    fn advance(&mut self, n_bytes: usize) {
        self.advance += n_bytes;
        self.buffer = &self.buffer[n_bytes..];
    }

    fn consume(&mut self, s: impl AsRef<[u8]>) -> Result<MustUse<bool>> {
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

        if let Some(x) = s.strip_prefix(b"xml") {
            let next = match x.split_first() {
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
    fn maybe_peek_byte(&self) -> Option<u8> {
        self.buffer.first().copied()
    }

    /// Peeks at the next byte and returns true if:
    ///
    /// - the next byte is '!' or '?', indicating a potential special tag (`<!...`, `<?...`)
    /// - the buffer is empty (no next byte currently available, so we have to check for
    ///   special tags the normal way)
    fn maybe_special_tag_start_char(&self) -> MustUse<bool> {
        match self.maybe_peek_byte() {
            Some(b) => MustUse(b == b'!' || b == b'?'),
            None => MustUse(true),
        }
    }

    fn require_or_else(&mut self, s: &[u8], e: impl FnOnce(usize) -> Error) -> Result<()> {
        if *abandon!(self.consume(s)) {
            Ok(())
        } else {
            Err(e(self.absolute_location()))
        }
    }

    fn require(&mut self, token: &[u8]) -> Result<()> {
        self.require_or_else(token, |location| {
            RequiredTokenMissingSnafu {
                token: RequiredToken::from_token(token),
                location,
            }
            .build()
        })
    }

    fn require_quote(&mut self) -> Result<Quote> {
        if *abandon!(self.consume(Quote::Double)) {
            Ok(Quote::Double)
        } else if *abandon!(self.consume(Quote::Single)) {
            Ok(Quote::Single)
        } else {
            let location = self.absolute_location();
            ExpectedSingleOrDoubleQuoteSnafu { location }.fail()
        }
    }

    fn attribute_value(&mut self, quote_style: Quote) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        match memchr::memchr3(b'<', b'&', quote_style.to_ascii_char(), s) {
            Some(x) => Ok(s[..x].xml_chars().complete_thing()),
            None => Ok(s.xml_chars().partial_thing()),
        }
    }

    /// Contrary to [`attribute_value`], this does not allow for
    /// less-than or ampersands inside the value.
    fn plain_attribute_value(&mut self, quote_style: Quote) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        match memchr::memchr(quote_style.to_ascii_char(), s) {
            Some(x) => Ok(s[..x].xml_chars().complete_thing()),
            None => Ok(s.xml_chars().partial_thing()),
        }
    }

    /// Anything that's not `<` or `&` so long as it doesn't include `]]>`
    fn char_data(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        Ok(s.char_data().partial_thing())
    }

    /// Anything that's not `]]>`
    fn cdata(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.min_bytes(3));

        match memmem::find(s, b"]]>") {
            Some(offset) => Ok(s[..offset].xml_chars().complete_thing()),
            None => {
                // Once for each `]`
                let s = s.strip_suffix(b"]").unwrap_or(s);
                let s = s.strip_suffix(b"]").unwrap_or(s);
                Ok(s.xml_chars().partial_thing())
            }
        }
    }

    fn processing_instruction_value(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.min_bytes(2));

        match memmem::find(s, b"?>") {
            Some(offset) => Ok(s[..offset].xml_chars().complete_thing()),
            None => {
                let s = s.strip_suffix(b"?").unwrap_or(s);
                Ok(s.xml_chars().partial_thing())
            }
        }
    }

    fn comment(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.min_bytes(2));

        match memmem::find(s, b"--") {
            Some(offset) => Ok(s[..offset].xml_chars().complete_thing()),
            None => {
                let s = s.strip_suffix(b"-").unwrap_or(s);
                Ok(s.xml_chars().partial_thing())
            }
        }
    }

    // Note that space amounts can be unbounded, which means that any
    // use of this should likely occur at the beginning of a state
    // dispatch.
    fn consume_space(&mut self) -> Result<usize> {
        let (s, r) = self.buffer.xml_space();

        let all_space = r.is_empty();

        self.advance(s.len());
        ensure!(
            !all_space,
            NeedsMoreInputSnafu {
                advance: self.advance
            }
        );

        Ok(s.len())
    }

    fn reference_decimal(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        Ok(s.reference_decimal().partial_thing())
    }

    fn reference_hex(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        Ok(s.reference_hex().partial_thing())
    }

    fn ncname(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        Ok(s.nc_name().partial_thing())
    }

    fn ncname_continuation(&self) -> Result<Streaming<&'b str>> {
        let s = abandon!(self.some_str());

        Ok(s.nc_name_continuation().partial_thing())
    }

    fn into_token_context(self) -> TokenContext {
        TokenContext { pre: self.advance }
    }
}

#[ext]
impl<'b> (&'b str, &[u8]) {
    fn partial_thing(self) -> Streaming<&'b str> {
        let (value, remainder) = self;

        if remainder.enough_to_parse() || value.is_empty() {
            Streaming::Complete(value)
        } else {
            Streaming::Partial(value)
        }
    }

    fn complete_thing(self) -> Streaming<&'b str> {
        let (value, _remainder) = self;
        Streaming::Complete(value)
    }
}

// There are a number of `*Space` states. Larger concepts may contain
// optional whitespace which may fill out the rest of the buffer. We
// need to be able to exit our loop, allow the user to refill the
// buffer, then resume parsing without losing our place. Each unique
// state provides a resumption point.
#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
enum State {
    #[default]
    Initial,

    AfterDeclarationOpen,
    AfterDeclarationOpenSpace,

    AfterDeclarationVersionAttribute,
    AfterDeclarationVersionAttributeSpace,
    AfterDeclarationVersionAttributeEquals,
    AfterDeclarationVersionAttributeEqualsSpace,
    StreamDeclarationVersion(Quote),
    AfterDeclarationVersionValue(Quote),
    AfterDeclarationVersion,
    AfterDeclarationVersionSpace,

    AfterDeclarationEncodingAttribute,
    AfterDeclarationEncodingAttributeSpace,
    AfterDeclarationEncodingAttributeEquals,
    AfterDeclarationEncodingAttributeEqualsSpace,
    StreamDeclarationEncoding(Quote),
    AfterDeclarationEncodingValue(Quote),
    AfterDeclarationEncoding,
    AfterDeclarationEncodingSpace,

    AfterDeclarationStandaloneAttribute,
    AfterDeclarationStandaloneAttributeSpace,
    AfterDeclarationStandaloneAttributeEquals,
    AfterDeclarationStandaloneAttributeEqualsSpace,
    StreamDeclarationStandalone(Quote),
    AfterDeclarationStandaloneValue(Quote),
    AfterDeclarationStandalone,
    AfterDeclarationStandaloneSpace,

    StreamElementOpenName,
    AfterElementOpenName,
    StreamElementOpenNameSuffix,
    AfterElementOpenNameSuffix,
    AfterElementOpenNameComplete,
    AfterElementOpenNameRequiredSpace,
    AfterElementOpenNameSpace,

    StreamAttributeName,
    AfterAttributeName,
    StreamAttributeNameSuffix,
    AfterAttributeNameSuffix,
    AfterAttributeNameComplete,
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
    StreamElementCloseNameSuffix,
    AfterElementCloseNameSuffix,
    AfterElementCloseNameComplete,
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
    fn to_ascii_char(self) -> u8 {
        match self {
            Self::Single => b'\'',
            Self::Double => b'"',
        }
    }
}

impl AsRef<[u8]> for Quote {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::Single => b"'",
            Self::Double => b"\"",
        }
    }
}

/// Indicates how many bytes of the buffer should be skipped before the token (`pre`).
#[derive(Debug, Copy, Clone)]
pub struct TokenContext {
    pub pre: usize,
}

type IndexTokenInner<'a> = UniformToken<Streaming<&'a str>>;
type IndexToken<'a> = (TokenContext, IndexTokenInner<'a>);

fn token_length(t: IndexTokenInner<'_>) -> usize {
    let v = match t {
        Token::DeclarationStart(l) => *l.unify(),
        Token::DeclarationEncoding(l) => *l.unify(),
        Token::DeclarationStandalone(l) => *l.unify(),
        Token::DeclarationClose => "",
        Token::ElementOpenStart(l) => *l.unify(),
        Token::ElementOpenStartSuffix(l) => *l.unify(),
        Token::ElementOpenStartComplete => "",
        Token::ElementOpenEnd => "",
        Token::ElementSelfClose => "",
        Token::ElementClose(l) => *l.unify(),
        Token::ElementCloseSuffix(l) => *l.unify(),
        Token::ElementCloseComplete => "",
        Token::AttributeStart(l) => *l.unify(),
        Token::AttributeStartSuffix(l) => *l.unify(),
        Token::AttributeStartComplete => "",
        Token::AttributeValueLiteral(l) => *l.unify(),
        Token::AttributeValueReferenceNamed(l) => *l.unify(),
        Token::AttributeValueReferenceDecimal(l) => *l.unify(),
        Token::AttributeValueReferenceHex(l) => *l.unify(),
        Token::AttributeValueEnd => "",
        Token::CharData(l) => *l.unify(),
        Token::CData(l) => *l.unify(),
        Token::ReferenceNamed(l) => *l.unify(),
        Token::ReferenceDecimal(l) => *l.unify(),
        Token::ReferenceHex(l) => *l.unify(),
        Token::ProcessingInstructionStart(l) => *l.unify(),
        Token::ProcessingInstructionValue(l) => *l.unify(),
        Token::ProcessingInstructionEnd => "",
        Token::Comment(l) => *l.unify(),
    };

    v.len()
}

#[derive(Debug, Default)]
pub struct CoreParser {
    state: State,
}

macro_rules! dispatch_namespaced_name {
    ($s:ident, $t:ident) => {
        paste::paste! {
            #[inline]
            fn [<dispatch_stream_ $s>]<'b>(
                &mut self,
                buffer: BufAdvance<'b>,
                f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
            ) -> Result<IndexToken<'b>> {
                self.stream_from_buffer(buffer, f, State::[<After $s:camel>], Token::[<$t:camel>])
            }

            #[inline]
            fn [<dispatch_after_ $s>]<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
                if *buffer.consume(":")? {
                    self.ratchet(State::[<Stream $s:camel Suffix>], &mut buffer);
                    self.[<dispatch_stream_ $s _suffix>](buffer, |b| b.ncname())
                } else {
                    self.ratchet(State::[<After $s:camel Suffix>], &mut buffer);
                    self.[<dispatch_after_ $s _suffix>](buffer)
                }
            }

            #[inline]
            fn [<dispatch_stream_ $s _suffix>]<'b>(
                &mut self,
                buffer: BufAdvance<'b>,
                f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
            ) -> Result<IndexToken<'b>> {
                self.stream_from_buffer(buffer, f, State::[<After $s:camel Suffix>], Token::[<$t:camel Suffix>])
            }

            #[inline]
            fn [<dispatch_after_ $s _suffix>]<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
                self.ratchet(State::[<After $s:camel Complete>], &mut buffer);
                let token_ctx = buffer.into_token_context();
                Ok((token_ctx, Token::[<$t:camel Complete>]))
            }
        }
    }
}

macro_rules! dispatch_eq_value {
    ($k:ident $($suffix:ident)?) => {
        paste::paste! {
            #[inline]
            fn [<dispatch_after_ $k $(_ $suffix)?>]<'b>(&mut self, buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
                self.consume_space(
                    buffer,
                    State::[<After $k:camel Space>],
                    Self::[<dispatch_after_ $k _space>],
                )
            }

            fn [<dispatch_after_ $k _space>]<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
                use State::*;

                buffer.require(b"=")?;

                self.ratchet([<After $k:camel Equals>], &mut buffer);
                self.[<dispatch_after_ $k _equals>](buffer)
            }

            fn [<dispatch_after_ $k _equals>]<'b>(&mut self, buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
                self.consume_space(
                    buffer,
                    State::[<After $k:camel EqualsSpace>],
                    Self::[<dispatch_after_ $k _equals_space>],
                )
            }
        }
    };
}

impl CoreParser {
    pub fn new() -> Self {
        Default::default()
    }

    // When we transition state without returning yet, we need to
    // update where we should rollback to.
    //
    // Possible future perf: should only be needed when we immediately
    // jump to another dispatch function. We also don't need to
    // re-set the state at the start of `next`.
    #[inline]
    fn ratchet(&mut self, state: State, buffer: &mut BufAdvance<'_>) {
        self.state = state;
        buffer.checkpoint();
    }

    #[inline]
    pub fn next_token<'b>(&mut self, buffer: &'b [u8], exhausted: bool) -> Result<IndexToken<'b>> {
        use State::*;

        let buffer = BufAdvance::new(buffer, exhausted);

        match self.state {
            Initial => self.dispatch_initial(buffer),

            AfterDeclarationOpen => self.dispatch_after_declaration_open(buffer),
            AfterDeclarationOpenSpace => self.dispatch_after_declaration_open_space(buffer),

            AfterDeclarationVersionAttribute => {
                self.dispatch_after_declaration_version_attribute(buffer)
            }
            AfterDeclarationVersionAttributeSpace => {
                self.dispatch_after_declaration_version_attribute_space(buffer)
            }
            AfterDeclarationVersionAttributeEquals => {
                self.dispatch_after_declaration_version_attribute_equals(buffer)
            }
            AfterDeclarationVersionAttributeEqualsSpace => {
                self.dispatch_after_declaration_version_attribute_equals_space(buffer)
            }
            StreamDeclarationVersion(quote) => {
                self.dispatch_stream_declaration_version(buffer, quote)
            }
            AfterDeclarationVersionValue(quote) => {
                self.dispatch_after_declaration_version_value(buffer, quote)
            }
            AfterDeclarationVersion => self.dispatch_after_declaration_version(buffer),
            AfterDeclarationVersionSpace => self.dispatch_after_declaration_version_space(buffer),

            AfterDeclarationEncodingAttribute => {
                self.dispatch_after_declaration_encoding_attribute(buffer)
            }
            AfterDeclarationEncodingAttributeSpace => {
                self.dispatch_after_declaration_encoding_attribute_space(buffer)
            }
            AfterDeclarationEncodingAttributeEquals => {
                self.dispatch_after_declaration_encoding_attribute_equals(buffer)
            }
            AfterDeclarationEncodingAttributeEqualsSpace => {
                self.dispatch_after_declaration_encoding_attribute_equals_space(buffer)
            }
            StreamDeclarationEncoding(quote) => {
                self.dispatch_stream_declaration_encoding(buffer, quote)
            }
            AfterDeclarationEncodingValue(quote) => {
                self.dispatch_after_declaration_encoding_value(buffer, quote)
            }
            AfterDeclarationEncoding => self.dispatch_after_declaration_encoding(buffer),
            AfterDeclarationEncodingSpace => self.dispatch_after_declaration_encoding_space(buffer),

            AfterDeclarationStandaloneAttribute => {
                self.dispatch_after_declaration_standalone_attribute(buffer)
            }
            AfterDeclarationStandaloneAttributeSpace => {
                self.dispatch_after_declaration_standalone_attribute_space(buffer)
            }
            AfterDeclarationStandaloneAttributeEquals => {
                self.dispatch_after_declaration_standalone_attribute_equals(buffer)
            }
            AfterDeclarationStandaloneAttributeEqualsSpace => {
                self.dispatch_after_declaration_standalone_attribute_equals_space(buffer)
            }
            StreamDeclarationStandalone(quote) => {
                self.dispatch_stream_declaration_standalone(buffer, quote)
            }
            AfterDeclarationStandaloneValue(quote) => {
                self.dispatch_after_declaration_standalone_value(buffer, quote)
            }
            AfterDeclarationStandalone => self.dispatch_after_declaration_standalone(buffer),
            AfterDeclarationStandaloneSpace => {
                self.dispatch_after_declaration_standalone_space(buffer)
            }

            StreamElementOpenName => {
                self.dispatch_stream_element_open_name(buffer, |b| b.ncname_continuation())
            }
            AfterElementOpenName => self.dispatch_after_element_open_name(buffer),
            StreamElementOpenNameSuffix => {
                self.dispatch_stream_element_open_name_suffix(buffer, |b| b.ncname_continuation())
            }
            AfterElementOpenNameSuffix => self.dispatch_after_element_open_name_suffix(buffer),
            AfterElementOpenNameComplete => self.dispatch_after_element_open_name_complete(buffer),
            AfterElementOpenNameRequiredSpace => {
                self.dispatch_after_element_open_name_required_space(buffer)
            }
            AfterElementOpenNameSpace => self.dispatch_after_element_open_name_space(buffer),

            StreamAttributeName => {
                self.dispatch_stream_attribute_name(buffer, |b| b.ncname_continuation())
            }
            AfterAttributeName => self.dispatch_after_attribute_name(buffer),
            StreamAttributeNameSuffix => {
                self.dispatch_stream_attribute_name_suffix(buffer, |b| b.ncname_continuation())
            }
            AfterAttributeNameSuffix => self.dispatch_after_attribute_name_suffix(buffer),
            AfterAttributeNameComplete => self.dispatch_after_attribute_name_complete(buffer),
            AfterAttributeNameSpace => self.dispatch_after_attribute_name_space(buffer),
            AfterAttributeNameEquals => self.dispatch_after_attribute_name_equals(buffer),
            AfterAttributeNameEqualsSpace => {
                self.dispatch_after_attribute_name_equals_space(buffer)
            }
            AfterAttributeOpenQuote(quote) => {
                self.dispatch_after_attribute_open_quote(buffer, quote)
            }
            StreamAttributeValueLiteral(quote) => {
                self.dispatch_stream_attribute_value_literal(buffer, quote)
            }
            StreamAttributeValueReferenceHex(quote) => {
                self.dispatch_stream_attribute_value_reference_hex(buffer, quote)
            }
            StreamAttributeValueReferenceDecimal(quote) => {
                self.dispatch_stream_attribute_value_reference_decimal(buffer, quote)
            }
            StreamAttributeValueReferenceNamed(quote) => self
                .dispatch_stream_attribute_value_reference_named(buffer, quote, |b| {
                    b.ncname_continuation()
                }),
            AfterAttributeValueReference(quote) => {
                self.dispatch_after_attribute_value_reference(buffer, quote)
            }

            StreamElementCloseName => {
                self.dispatch_stream_element_close_name(buffer, |b| b.ncname_continuation())
            }
            AfterElementCloseName => self.dispatch_after_element_close_name(buffer),
            StreamElementCloseNameSuffix => {
                self.dispatch_stream_element_close_name_suffix(buffer, |b| b.ncname_continuation())
            }
            AfterElementCloseNameSuffix => self.dispatch_after_element_close_name_suffix(buffer),
            AfterElementCloseNameComplete => {
                self.dispatch_after_element_close_name_complete(buffer)
            }
            AfterElementCloseNameSpace => self.dispatch_after_element_close_name_space(buffer),

            StreamCharData => self.dispatch_stream_char_data(buffer),
            StreamCData => self.dispatch_stream_cdata(buffer),
            AfterCData => self.dispatch_after_cdata(buffer),

            StreamReferenceNamed => {
                self.dispatch_stream_reference_named(buffer, |b| b.ncname_continuation())
            }
            StreamReferenceDecimal => self.dispatch_stream_reference_decimal(buffer),
            StreamReferenceHex => self.dispatch_stream_reference_hex(buffer),
            AfterReference => self.dispatch_after_reference(buffer),

            StreamProcessingInstructionName => self
                .dispatch_stream_processing_instruction_name(buffer, |b| b.ncname_continuation()),
            AfterProcessingInstructionName => {
                self.dispatch_after_processing_instruction_name(buffer)
            }
            AfterProcessingInstructionNameRequiredSpace => {
                self.dispatch_after_processing_instruction_name_required_space(buffer)
            }
            AfterProcessingInstructionNameSpace => {
                self.dispatch_after_processing_instruction_name_space(buffer)
            }
            StreamProcessingInstructionValue => {
                self.dispatch_stream_processing_instruction_value(buffer)
            }
            AfterProcessingInstructionValue => {
                self.dispatch_after_processing_instruction_value(buffer)
            }

            StreamComment => self.dispatch_stream_comment(buffer),
            AfterComment => self.dispatch_after_comment(buffer),
        }
    }

    fn finish(&mut self) -> Result<()> {
        ensure!(
            matches!(self.state, State::Initial | State::StreamCharData),
            IncompleteXmlSnafu
        );

        Ok(())
    }

    #[inline]
    fn dispatch_initial<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume("<")? {
            if *buffer.consume("/")? {
                self.ratchet(StreamElementCloseName, &mut buffer);
                return self.dispatch_stream_element_close_name(buffer, |b| b.ncname());
            }

            if *buffer.maybe_special_tag_start_char() {
                if *buffer.consume("![CDATA[")? {
                    self.ratchet(StreamCData, &mut buffer);
                    return self.dispatch_stream_cdata(buffer);
                }

                if *buffer.consume("!--")? {
                    self.ratchet(StreamComment, &mut buffer);
                    return self.dispatch_stream_comment(buffer);
                }

                if *buffer.consume("?")? {
                    if *buffer.consume_xml()? {
                        self.ratchet(AfterDeclarationOpen, &mut buffer);
                        return self.dispatch_after_declaration_open(buffer);
                    } else {
                        self.ratchet(StreamProcessingInstructionName, &mut buffer);
                        return self
                            .dispatch_stream_processing_instruction_name(buffer, |b| b.ncname());
                    }
                }
            }

            // regular open tag
            self.ratchet(StreamElementOpenName, &mut buffer);
            return self.dispatch_stream_element_open_name(buffer, |b| b.ncname());
        }

        if *buffer.consume("&#x")? {
            self.ratchet(StreamReferenceHex, &mut buffer);
            self.dispatch_stream_reference_hex(buffer)
        } else if *buffer.consume("&#")? {
            self.ratchet(StreamReferenceDecimal, &mut buffer);
            self.dispatch_stream_reference_decimal(buffer)
        } else if *buffer.consume("&")? {
            self.ratchet(StreamReferenceNamed, &mut buffer);
            self.dispatch_stream_reference_named(buffer, |b| b.ncname())
        } else {
            let v = buffer.char_data()?;

            match v {
                Streaming::Complete(s) | Streaming::Partial(s) if s.is_empty() => {
                    let location = buffer.absolute_location();
                    return InvalidXmlSnafu { location }.fail();
                }

                Streaming::Complete(_) => {
                    // Stay in the current state
                }

                Streaming::Partial(_) => {
                    self.ratchet(StreamCharData, &mut buffer);
                }
            }

            let token_ctx = buffer.into_token_context();

            Ok((token_ctx, CharData(v)))
        }
    }

    fn dispatch_after_declaration_open<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterDeclarationOpenSpace,
            Self::dispatch_after_declaration_open_space,
        )
    }

    fn dispatch_after_declaration_open_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        buffer.require(b"version")?;
        self.ratchet(State::AfterDeclarationVersionAttribute, &mut buffer);
        self.dispatch_after_declaration_version_attribute(buffer)
    }

    dispatch_eq_value!(declaration_version_attribute);

    fn dispatch_after_declaration_version_attribute_equals_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        let quote = buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationVersion(quote), &mut buffer);
        self.dispatch_stream_declaration_version(buffer, quote)
    }

    fn dispatch_stream_declaration_version<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        let value = buffer.plain_attribute_value(quote)?;

        if value.is_complete() {
            self.ratchet(AfterDeclarationVersionValue(quote), &mut buffer);
        }

        let token_ctx = buffer.into_token_context();

        Ok((token_ctx, DeclarationStart(value)))
    }

    fn dispatch_after_declaration_version_value<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(quote.as_ref())?;
        self.ratchet(AfterDeclarationVersion, &mut buffer);

        self.dispatch_after_declaration_version(buffer)
    }

    fn dispatch_after_declaration_version<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterDeclarationVersionSpace,
            Self::dispatch_after_declaration_version_space,
        )
    }

    fn dispatch_after_declaration_version_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        // TODO: this should require that we've seen a space in order to be allowed
        if *buffer.consume("encoding")? {
            self.ratchet(AfterDeclarationEncodingAttribute, &mut buffer);
            self.dispatch_after_declaration_encoding_attribute(buffer)
        } else {
            self.ratchet(AfterDeclarationEncodingSpace, &mut buffer);
            self.dispatch_after_declaration_encoding_space(buffer)
        }
    }

    dispatch_eq_value!(declaration_encoding_attribute);

    fn dispatch_after_declaration_encoding_attribute_equals_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        let quote = buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationEncoding(quote), &mut buffer);
        self.dispatch_stream_declaration_encoding(buffer, quote)
    }

    fn dispatch_stream_declaration_encoding<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        let value = buffer.plain_attribute_value(quote)?;

        if value.is_complete() {
            self.ratchet(AfterDeclarationEncodingValue(quote), &mut buffer);
        }

        let token_ctx = buffer.into_token_context();

        Ok((token_ctx, DeclarationEncoding(value)))
    }

    fn dispatch_after_declaration_encoding_value<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(quote.as_ref())?;

        self.ratchet(AfterDeclarationEncoding, &mut buffer);
        self.dispatch_after_declaration_encoding(buffer)
    }

    fn dispatch_after_declaration_encoding<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterDeclarationEncodingSpace,
            Self::dispatch_after_declaration_encoding_space,
        )
    }

    fn dispatch_after_declaration_encoding_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        if *buffer.consume("standalone")? {
            self.ratchet(AfterDeclarationStandaloneAttribute, &mut buffer);
            self.dispatch_after_declaration_standalone_attribute(buffer)
        } else {
            self.ratchet(AfterDeclarationStandaloneSpace, &mut buffer);
            self.dispatch_after_declaration_standalone_space(buffer)
        }
    }

    dispatch_eq_value!(declaration_standalone_attribute);

    fn dispatch_after_declaration_standalone_attribute_equals_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        let quote = buffer.require_quote()?;

        self.ratchet(State::StreamDeclarationStandalone(quote), &mut buffer);
        self.dispatch_stream_declaration_standalone(buffer, quote)
    }

    fn dispatch_stream_declaration_standalone<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        let value = buffer.plain_attribute_value(quote)?;

        if value.is_complete() {
            self.ratchet(AfterDeclarationStandaloneValue(quote), &mut buffer);
        }

        let token_ctx = buffer.into_token_context();

        Ok((token_ctx, DeclarationStandalone(value)))
    }

    fn dispatch_after_declaration_standalone_value<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(quote.as_ref())?;

        self.ratchet(AfterDeclarationStandalone, &mut buffer);
        self.dispatch_after_declaration_standalone(buffer)
    }

    fn dispatch_after_declaration_standalone<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterDeclarationStandaloneSpace,
            Self::dispatch_after_declaration_standalone_space,
        )
    }

    fn dispatch_after_declaration_standalone_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        buffer.require(b"?>")?;

        self.ratchet(Initial, &mut buffer);
        let token_ctx = buffer.into_token_context();
        Ok((token_ctx, DeclarationClose))
    }

    dispatch_namespaced_name!(element_open_name, ElementOpenStart);

    fn dispatch_after_element_open_name_complete<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume("/>")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ElementSelfClose))
        } else if *buffer.consume(">")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ElementOpenEnd))
        } else {
            self.require_space(
                buffer,
                AfterElementOpenNameRequiredSpace,
                Self::dispatch_after_element_open_name_required_space,
            )
        }
    }

    #[inline]
    fn dispatch_after_element_open_name_required_space<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterElementOpenNameSpace,
            Self::dispatch_after_element_open_name_space,
        )
    }

    #[inline]
    fn dispatch_after_element_open_name_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume("/>")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ElementSelfClose))
        } else if *buffer.consume(">")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ElementOpenEnd))
        } else {
            self.ratchet(StreamAttributeName, &mut buffer);
            self.dispatch_stream_attribute_name(buffer, |b| b.ncname())
        }
    }

    dispatch_namespaced_name!(attribute_name, AttributeStart);
    dispatch_eq_value!(attribute_name complete);

    fn dispatch_after_attribute_name_equals_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        let quote = buffer.require_quote()?;
        self.ratchet(AfterAttributeOpenQuote(quote), &mut buffer);
        self.dispatch_after_attribute_open_quote(buffer, quote)
    }

    #[inline]
    fn dispatch_after_attribute_open_quote<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume(quote)? {
            self.ratchet(AfterElementOpenNameComplete, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, AttributeValueEnd))
        } else if *buffer.consume(b"&#x")? {
            self.ratchet(StreamAttributeValueReferenceHex(quote), &mut buffer);
            self.dispatch_stream_attribute_value_reference_hex(buffer, quote)
        } else if *buffer.consume(b"&#")? {
            self.ratchet(StreamAttributeValueReferenceDecimal(quote), &mut buffer);
            self.dispatch_stream_attribute_value_reference_decimal(buffer, quote)
        } else if *buffer.consume(b"&")? {
            self.ratchet(StreamAttributeValueReferenceNamed(quote), &mut buffer);
            self.dispatch_stream_attribute_value_reference_named(buffer, quote, |b| b.ncname())
        } else {
            let value = buffer.attribute_value(quote)?;

            match value {
                Streaming::Complete(s) | Streaming::Partial(s) if s.is_empty() => {
                    return InvalidCharacterInAttributeSnafu {
                        location: buffer.absolute_location(),
                    }
                    .fail();
                }

                Streaming::Complete(_) => {
                    // Stay in the current state
                }

                Streaming::Partial(_) => {
                    self.ratchet(StreamAttributeValueLiteral(quote), &mut buffer);
                }
            };

            let token_ctx = buffer.into_token_context();

            Ok((token_ctx, AttributeValueLiteral(value)))
        }
    }

    // -- todo: copy-pastad
    fn dispatch_stream_attribute_value_reference_named<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
        quote: Quote,
        f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            f,
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceNamed,
        )
    }

    fn dispatch_stream_attribute_value_reference_decimal<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            |b| b.reference_decimal(),
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceDecimal,
        )
    }

    fn dispatch_stream_attribute_value_reference_hex<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            |b| b.reference_hex(),
            State::AfterAttributeValueReference(quote),
            Token::AttributeValueReferenceHex,
        )
    }

    fn dispatch_after_attribute_value_reference<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(b";")?;
        self.ratchet(AfterAttributeOpenQuote(quote), &mut buffer);
        self.dispatch_after_attribute_open_quote(buffer, quote)
    }
    // ---

    #[inline]
    fn dispatch_stream_attribute_value_literal<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        quote: Quote,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        let value = buffer.attribute_value(quote)?;

        if value.is_complete() {
            self.ratchet(AfterAttributeOpenQuote(quote), &mut buffer);
        }

        let token_ctx = buffer.into_token_context();

        Ok((token_ctx, AttributeValueLiteral(value)))
    }

    dispatch_namespaced_name!(element_close_name, ElementClose);

    #[inline]
    fn dispatch_after_element_close_name_complete<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterElementCloseNameSpace,
            Self::dispatch_after_element_close_name_space,
        )
    }

    #[inline]
    fn dispatch_after_element_close_name_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(b">")?;

        self.ratchet(Initial, &mut buffer);
        self.dispatch_initial(buffer)
    }

    #[inline]
    fn dispatch_stream_reference_named<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
        f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(buffer, f, State::AfterReference, Token::ReferenceNamed)
    }

    fn dispatch_stream_reference_decimal<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            |b| b.reference_decimal(),
            State::AfterReference,
            Token::ReferenceDecimal,
        )
    }

    fn dispatch_stream_reference_hex<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            |b| b.reference_hex(),
            State::AfterReference,
            Token::ReferenceHex,
        )
    }

    #[inline]
    fn dispatch_after_reference<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(b";")?;
        self.ratchet(Initial, &mut buffer);
        self.dispatch_initial(buffer)
    }

    fn dispatch_stream_processing_instruction_name<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
        f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            f,
            State::AfterProcessingInstructionName,
            Token::ProcessingInstructionStart,
        )
    }

    fn dispatch_after_processing_instruction_name<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume("?>")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ProcessingInstructionEnd))
        } else {
            self.require_space(
                buffer,
                AfterProcessingInstructionNameRequiredSpace,
                Self::dispatch_after_processing_instruction_name_required_space,
            )
        }
    }

    fn dispatch_after_processing_instruction_name_required_space<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.consume_space(
            buffer,
            State::AfterProcessingInstructionNameSpace,
            Self::dispatch_after_processing_instruction_name_space,
        )
    }

    fn dispatch_after_processing_instruction_name_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        if *buffer.consume("?>")? {
            self.ratchet(Initial, &mut buffer);
            let token_ctx = buffer.into_token_context();
            Ok((token_ctx, ProcessingInstructionEnd))
        } else {
            self.ratchet(StreamProcessingInstructionValue, &mut buffer);
            self.dispatch_stream_processing_instruction_value(buffer)
        }
    }

    fn dispatch_stream_processing_instruction_value<'b>(
        &mut self,
        buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(
            buffer,
            |b| b.processing_instruction_value(),
            State::AfterProcessingInstructionValue,
            Token::ProcessingInstructionValue,
        )
    }

    fn dispatch_after_processing_instruction_value<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
    ) -> Result<IndexToken<'b>> {
        use {State::*, Token::*};

        buffer.require(b"?>")?;

        self.ratchet(Initial, &mut buffer);
        let token_ctx = buffer.into_token_context();
        Ok((token_ctx, ProcessingInstructionEnd))
    }

    #[inline]
    fn dispatch_stream_char_data<'b>(&mut self, buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(buffer, |b| b.char_data(), State::Initial, Token::CharData)
    }

    fn dispatch_stream_cdata<'b>(&mut self, buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(buffer, |b| b.cdata(), State::AfterCData, Token::CData)
    }

    fn dispatch_after_cdata<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require(b"]]>")?;

        self.ratchet(Initial, &mut buffer);
        self.dispatch_initial(buffer)
    }

    fn dispatch_stream_comment<'b>(&mut self, buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        self.stream_from_buffer(buffer, |b| b.comment(), State::AfterComment, Token::Comment)
    }

    fn dispatch_after_comment<'b>(&mut self, mut buffer: BufAdvance<'b>) -> Result<IndexToken<'b>> {
        use State::*;

        buffer.require_or_else(b"-->", |location| {
            DoubleHyphenInCommentSnafu { location }.build()
        })?;

        self.ratchet(Initial, &mut buffer);
        self.dispatch_initial(buffer)
    }

    // ----------

    fn require_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        next_state: State,
        next_state_fn: impl FnOnce(&mut Self, BufAdvance<'b>) -> Result<IndexToken<'b>>,
    ) -> Result<IndexToken<'b>> {
        match buffer.consume_space() {
            Ok(0) => RequiredSpaceMissingSnafu {
                location: buffer.absolute_location(),
            }
            .fail(),
            Ok(_) => {
                self.ratchet(next_state, &mut buffer);
                next_state_fn(self, buffer)
            }
            Err(e @ Error::NeedsMoreInput { advance: 0 }) => Err(e),
            Err(e @ Error::NeedsMoreInput { .. }) => {
                self.ratchet(next_state, &mut buffer);
                Err(e)
            }
            Err(e) => Err(e),
        }
    }

    fn consume_space<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        next_state: State,
        next_state_fn: impl FnOnce(&mut Self, BufAdvance<'b>) -> Result<IndexToken<'b>>,
    ) -> Result<IndexToken<'b>> {
        buffer.consume_space()?;

        self.ratchet(next_state, &mut buffer);
        next_state_fn(self, buffer)
    }

    #[inline]
    fn stream_from_buffer<'b>(
        &mut self,
        mut buffer: BufAdvance<'b>,
        f: impl FnOnce(&mut BufAdvance<'b>) -> Result<Streaming<&'b str>>,
        next_state: State,
        create: impl FnOnce(Streaming<&'b str>) -> IndexTokenInner<'b>,
    ) -> Result<IndexToken<'b>> {
        let value = f(&mut buffer)?;

        if value.is_complete() {
            self.ratchet(next_state, &mut buffer);
        }

        let token_ctx = buffer.into_token_context();

        Ok((token_ctx, create(value)))
    }
}

// https://github.com/rust-lang/rust/issues/78149
#[must_use]
#[derive(Debug)]
struct MustUse<T>(T);

impl<T> ops::Deref for MustUse<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

impl<T> ops::DerefMut for MustUse<T> {
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
    DoubleQuote,
    SingleQuote,
}

impl RequiredToken {
    fn from_token(s: &[u8]) -> Self {
        match s {
            b"version" => Self::Version,
            b"=" => Self::Equals,
            b"?>" => Self::QuestionMarkClosingAngleBracket,
            b">" => Self::ClosingAngleBracket,
            b";" => Self::Semicolon,
            b"]]>" => Self::CDataEnd,
            b"\"" => Self::DoubleQuote,
            b"'" => Self::SingleQuote,
            _ => panic!("unknown token"),
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    NeedsMoreInput {
        advance: usize,
    },

    #[snafu(display("Expected the token {token:?} at byte {location}, but it was missing"))]
    RequiredTokenMissing {
        token: RequiredToken,
        location: usize,
    },

    #[snafu(display("Required space but it was missing"))]
    RequiredSpaceMissing {
        location: usize,
    },

    #[snafu(display(
        "Expected either a single or double quote around the attribute value at byte {location}"
    ))]
    ExpectedSingleOrDoubleQuote {
        location: usize,
    },

    #[snafu(display("An invalid character is inside an attribute at byte {location}"))]
    InvalidCharacterInAttribute {
        location: usize,
    },

    #[snafu(display("A double hyphen is inside a comment at byte {location}"))]
    DoubleHyphenInComment {
        location: usize,
    },

    #[snafu(display("The input data did not end in a valid state"))]
    IncompleteXml,

    #[snafu(display("The input data is not valid XML starting at byte {location}"))]
    InvalidXml {
        location: usize,
    },

    #[snafu(display("Unparsed data remains at byte {location}"))]
    ExtraData {
        location: usize,
    },
}

impl Error {
    fn advance_location(&mut self, delta: usize) {
        let location = match self {
            Error::NeedsMoreInput { .. } | Error::IncompleteXml => return,
            Error::RequiredTokenMissing { location, .. } => location,
            Error::RequiredSpaceMissing { location } => location,
            Error::ExpectedSingleOrDoubleQuote { location } => location,
            Error::InvalidCharacterInAttribute { location } => location,
            Error::DoubleHyphenInComment { location } => location,
            Error::InvalidXml { location } => location,
            Error::ExtraData { location } => location,
        };
        *location += delta;
    }

    fn needs_more_input(&self) -> Option<usize> {
        match self {
            Error::NeedsMoreInput { advance } => Some(*advance),
            _ => None,
        }
    }
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

#[derive(Debug)]
struct MyBufReader<R> {
    source: R,
    data: Box<[u8]>,
    meta: Meta,
}

impl<R> MyBufReader<R> {
    fn with_capacity(source: R, capacity: usize) -> Self {
        Self {
            source,
            data: vec![0; capacity].into(),
            meta: Default::default(),
        }
    }

    fn buffer(&mut self) -> (&[u8], &mut Meta) {
        let buf = &self.data[self.meta.valid_range()];

        (buf, &mut self.meta)
    }

    fn consumed(&self) -> usize {
        self.meta.consumed()
    }

    fn is_empty(&self) -> bool {
        self.meta.is_empty()
    }

    fn move_to_front(&mut self) {
        self.data.copy_within(self.meta.valid_range(), 0);
        self.meta.offset = 0;
    }
}

impl<R> MyBufReader<R>
where
    R: Read,
{
    fn top_off(&mut self) -> io::Result<usize> {
        let invalid_part = &mut self.data[self.meta.valid..];

        let n = self.source.read(invalid_part)?;
        self.meta.valid += n;

        Ok(n)
    }
}

#[derive(Debug, Default)]
struct Meta {
    offset: usize,
    valid: usize,
    consumed: usize,
}

impl Meta {
    fn valid_range(&self) -> ops::Range<usize> {
        let start = self.offset;
        let end = start + self.valid;

        start..end
    }

    fn is_empty(&self) -> bool {
        self.valid == 0
    }

    fn consumed(&self) -> usize {
        self.consumed
    }

    fn consume(&mut self, mut amt: usize) {
        amt = usize::min(amt, self.valid);

        self.consumed += amt;
        self.offset += amt;
        self.valid -= amt;
    }
}

#[derive(Debug)]
pub struct Parser<R> {
    source: MyBufReader<R>,
    parser: CoreParser,
    exhausted: bool,
}

impl<R> Parser<R>
where
    R: Read,
{
    pub fn new(source: R) -> Self {
        Self::with_buffer_capacity(source, DEFAULT_CAPACITY)
    }

    pub fn with_buffer_capacity(source: R, capacity: usize) -> Self {
        Self {
            source: MyBufReader::with_capacity(source, capacity),
            parser: CoreParser::new(),
            exhausted: false,
        }
    }

    pub fn next_token(&mut self) -> Option<Result<IndexTokenInner<'_>>> {
        let Self {
            source,
            parser,
            exhausted,
        } = self;

        let mut source = source;

        loop {
            polonius_the_crab::polonius!(|source| -> Option<Result<IndexTokenInner<'polonius>>> {
                let (buffer, meta) = source.buffer();

                match parser.next_token(buffer, *exhausted) {
                    Err(mut e) => {
                        if let Some(a) = e.needs_more_input() {
                            meta.consume(a);
                        } else {
                            e.advance_location(meta.consumed());
                            polonius_the_crab::polonius_return!(Some(Err(e)));
                        }
                    }

                    Ok((ctx, v)) => {
                        meta.consume(ctx.pre + token_length(v));
                        polonius_the_crab::polonius_return!(Some(Ok(v)));
                    }
                }
            });

            if *exhausted {
                return match parser.finish() {
                    Ok(()) => {
                        if source.is_empty() {
                            None
                        } else {
                            let location = source.consumed();
                            let e = ExtraDataSnafu { location }.fail();
                            Some(e)
                        }
                    }

                    Err(e) => Some(Err(e)),
                };
            }

            source.move_to_front();
            let n_new_bytes = source.top_off();

            match n_new_bytes {
                Ok(l) => {
                    *exhausted = l == 0;
                    continue;
                }

                Err(e) => panic!("TODO: report this IO error {e} / {e:?}"),
            }
        }
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
            fuse ElementOpenStartSuffix,
            pass ElementOpenStartComplete,
            pass ElementOpenEnd,
            pass ElementSelfClose,
            fuse ElementClose,
            fuse ElementCloseSuffix,
            pass ElementCloseComplete,
            fuse AttributeStart,
            fuse AttributeStartSuffix,
            pass AttributeStartComplete,
            stream AttributeValueLiteral,
            fuse AttributeValueReferenceNamed,
            fuse AttributeValueReferenceDecimal,
            fuse AttributeValueReferenceHex,
            pass AttributeValueEnd,
            stream CharData,
            stream CData,
            fuse ReferenceNamed,
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

#[derive(Debug, Default)]
struct FuseCore {
    buffer: String,
    current: Option<UniformToken<()>>,
}

impl FuseCore {
    fn push<'b, 't: 'b>(&'b mut self, t: IndexTokenInner<'t>) -> Option<FusedToken<'b>> {
        use {Streaming::*, Token::*};

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
                        buffer.push_str(v);
                        None
                    }
                    (Partial(v), Some($name(()))) => {
                        buffer.push_str(v);
                        None
                    }
                    (Complete(v), None) => Some($name(v)),
                    (Complete(v), Some($name(()))) => {
                        *current = None;
                        buffer.push_str(v);
                        Some($name(&buffer[..]))
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

    fn finish(&mut self) -> Result<Option<FusedToken<'_>>, FuseError> {
        match self.current.take() {
            Some(_) => IncompleteSnafu.fail(),
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

    pub fn next_token(&mut self) -> Option<Result<FusedToken<'_>, FuseError>> {
        let mut this = self;

        loop {
            polonius_the_crab::polonius!(
                |this| -> Option<Result<FusedToken<'polonius>, FuseError>> {
                    let t = match this.inner.next_token() {
                        Some(Ok(t)) => t,

                        Some(Err(e)) => polonius_the_crab::polonius_return!(Some(Err(e.into()))),

                        None => {
                            let v = match this.core.finish() {
                                Ok(v) => v.map(Ok),
                                Err(e) => Some(Err(e)),
                            };
                            polonius_the_crab::polonius_return!(v);
                        }
                    };

                    if let Some(t) = this.core.push(t) {
                        polonius_the_crab::polonius_return!(Some(Ok(t)));
                    }
                }
            )
        }
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

    impl TryIntoTokens for &str {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            self.as_bytes().try_into_tokens()
        }
    }

    impl TryIntoTokens for &[u8] {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            Parser::with_buffer_capacity(self, DEFAULT_CAPACITY).collect_owned()
        }
    }

    impl<const N: usize> TryIntoTokens for &[u8; N] {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            <&[u8] as TryIntoTokens>::try_into_tokens(self)
        }
    }

    struct WithCapacity<T>(T, usize);

    fn capacity<T>(c: usize) -> impl FnOnce(T) -> WithCapacity<T> {
        move |v| WithCapacity(v, c)
    }

    fn minimum_capacity<T>(v: T) -> WithCapacity<T> {
        WithCapacity(v, MINIMUM_CAPACITY)
    }

    impl<T> TryIntoTokens for WithCapacity<T>
    where
        T: AsRef<[u8]>,
    {
        type Error = Error;

        #[track_caller]
        fn try_into_tokens(self) -> Result<OwnedTokens, Self::Error> {
            Parser::with_buffer_capacity(self.0.as_ref(), self.1).collect_owned()
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
            ElementOpenStartComplete,
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
                ElementOpenStartComplete,
                ElementSelfClose,
            ]))
    }

    #[test]
    fn self_closed_element_with_one_attribute() -> Result {
        expect(r#"<alpha a="b"/>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementOpenStartComplete,
            AttributeStart(Complete("a")),
            AttributeStartComplete,
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
                ElementOpenStartComplete,
                AttributeStart(Partial("b01234567")),
                AttributeStart(Complete("890123456789")),
                AttributeStartComplete,
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
            ElementOpenStartComplete,
            AttributeStart(Complete("a")),
            AttributeStartComplete,
            AttributeValueLiteral(Complete("b")),
            AttributeValueEnd,
            AttributeStart(Complete("c")),
            AttributeStartComplete,
            AttributeValueLiteral(Complete("d")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn attribute_with_escaped_less_than_and_ampersand() -> Result {
        expect("<a b='&lt;&amp;' />").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            ElementOpenStartComplete,
            AttributeStart(Complete("b")),
            AttributeStartComplete,
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
            ElementOpenStartComplete,
            AttributeStart(Complete("b")),
            AttributeStartComplete,
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
            ElementOpenStartComplete,
            ElementOpenEnd,
            ElementClose(Complete("alpha")),
            ElementCloseComplete,
        ]))
    }

    #[test]
    fn element_with_no_children_small_capacity() -> Result {
        expect(r#"<a01234567890123456789></a01234567890123456789>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Partial("a01234567890123")),
                ElementOpenStart(Complete("456789")),
                ElementOpenStartComplete,
                ElementOpenEnd,
                ElementClose(Partial("a012345")),
                ElementClose(Complete("67890123456789")),
                ElementCloseComplete,
            ]))
    }

    #[test]
    fn element_with_one_child() -> Result {
        expect(r#"<alpha><beta /></alpha>"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementOpenStartComplete,
            ElementOpenEnd,
            ElementOpenStart(Complete("beta")),
            ElementOpenStartComplete,
            ElementSelfClose,
            ElementClose(Complete("alpha")),
            ElementCloseComplete,
        ]))
    }

    #[test]
    fn element_with_namespaced_name() -> Result {
        expect(r#"<l:alpha></l:alpha>"#).to(be_parsed_as([
            ElementOpenStart(Complete("l")),
            ElementOpenStartSuffix(Complete("alpha")),
            ElementOpenStartComplete,
            ElementOpenEnd,
            ElementClose(Complete("l")),
            ElementCloseSuffix(Complete("alpha")),
            ElementCloseComplete,
        ]))
    }

    #[test]
    fn attribute_with_namespaced_name() -> Result {
        expect(r#"<alpha xmlns:l="sxd" />"#).to(be_parsed_as([
            ElementOpenStart(Complete("alpha")),
            ElementOpenStartComplete,
            AttributeStart(Complete("xmlns")),
            AttributeStartSuffix(Complete("l")),
            AttributeStartComplete,
            AttributeValueLiteral(Complete("sxd")),
            AttributeValueEnd,
            ElementSelfClose,
        ]))
    }

    #[test]
    fn char_data() -> Result {
        expect("<a>b</a>").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            ElementOpenStartComplete,
            ElementOpenEnd,
            CharData(Complete("b")),
            ElementClose(Complete("a")),
            ElementCloseComplete,
        ]))
    }

    #[test]
    fn char_data_small_capacity() -> Result {
        expect(r#"<a>01234567890123456789</a>"#)
            .with(minimum_capacity)
            .to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                ElementOpenStartComplete,
                ElementOpenEnd,
                CharData(Partial("0123456789012")),
                CharData(Complete("3456789")),
                ElementClose(Complete("a")),
                ElementCloseComplete,
            ]))
    }

    #[test]
    fn char_data_with_close_square_bracket() -> Result {
        expect("<a>b]</a>").to(be_parsed_as([
            ElementOpenStart(Complete("a")),
            ElementOpenStartComplete,
            ElementOpenEnd,
            CharData(Complete("b]")),
            ElementClose(Complete("a")),
            ElementCloseComplete,
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
            ElementOpenStartComplete,
            ElementSelfClose,
            CharData(Partial("\r\n")),
        ]))
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        expect("\t <a></a>\r\n").to(be_parsed_as([
            CharData(Complete("\t ")),
            ElementOpenStart(Complete("a")),
            ElementOpenStartComplete,
            ElementOpenEnd,
            ElementClose(Complete("a")),
            ElementCloseComplete,
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
    fn comment_empty() -> Result {
        expect("<!---->").to(be_parsed_as([Comment(Complete(""))]))
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
                ElementOpenStartComplete,
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

            expect(input).with(capacity(32)).to(be_parsed_as([
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

            expect(input).with(capacity(32)).to(be_parsed_as([
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

            expect(input).with(capacity(32)).to(be_parsed_as([
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
            expect(input).with(capacity(32)).to(be_parsed_as([
                ElementOpenStart(Complete("a")),
                ElementOpenStartComplete,
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
                ElementOpenStartComplete,
                AttributeStart(Complete("b")),
                AttributeStartComplete,
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
                ElementOpenStartComplete,
                AttributeStart(Complete("b")),
                AttributeStartComplete,
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
            expect(input).with(capacity(32)).to(be_parsed_as([
                ElementClose(Complete("a")),
                ElementCloseComplete,
            ]))
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
    fn fail_declaration_invalid_version_close() -> Result {
        expect(r"<?xml version='1.0&?>").to(fail_parsing_with!(Error::IncompleteXml))
    }

    #[test]
    fn fail_declaration_invalid_encoding_close() -> Result {
        expect(r"<?xml version='1.0' encoding='UTF-8&?>")
            .to(fail_parsing_with!(Error::IncompleteXml))
    }

    #[test]
    fn fail_declaration_invalid_standalone_close() -> Result {
        expect(r"<?xml version='1.0' standalone='no&?>")
            .to(fail_parsing_with!(Error::IncompleteXml))
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
    fn fail_char_data_with_repeated_square_brackets() -> Result {
        expect("<a>b]]]></a>").to(fail_parsing_with!(Error::InvalidXml { location: 5 }))
    }

    mod malformed {
        use super::*;

        #[test]
        fn fail_disallowed_ascii_char() -> Result {
            let mut x = [b'a'; 20];
            x[19] = 0x07;

            expect(&x)
                .with(capacity(16))
                .to(fail_parsing_with!(Error::InvalidXml { location: 19 }))
        }

        #[test]
        fn fail_disallowed_multibyte_char() -> Result {
            let mut x = [b'a'; 20];
            x[17] = 0xEF;
            x[18] = 0xBF;
            x[19] = 0xBF;

            expect(&x)
                .with(capacity(16))
                .to(fail_parsing_with!(Error::InvalidXml { location: 17 }))
        }

        #[test]
        fn fail_non_utf_8() -> Result {
            expect(&[b'a', b'b', b'c', 0xFF])
                .to(fail_parsing_with!(Error::InvalidXml { location: 3 }))
        }

        #[test]
        fn xml_declaration_closing_quote() -> Result {
            // We were assuming that the closing quote was always present, but it could have been an
            // invalid XML character.

            expect(r#"<?xml version="1.0\u{7}?>"#).to(fail_parsing_with!(Error::IncompleteXml))
        }

        #[test]
        fn char_data_initial() -> Result {
            // Have enough bytes to look for a `]]>`, but those bytes aren't UTF-8 / XmlChar.
            let x = [0xf0, 0xff, 0x7f];

            expect(&x).to(fail_parsing_with!(Error::ExtraData { location: 0 }))
        }

        #[test]
        fn char_data_continuing() -> Result {
            // Have some initial valid UTF-8 / XmlChar, but followed by enough invalid bytes to look
            // for a `]]>`.
            let x = [b'a', 0xf0, 0x7f, 0x80];

            expect(&x).to(fail_parsing_with!(Error::ExtraData { location: 1 }))
        }

        #[test]
        fn processing_instruction_with_invalid_value() -> Result {
            let mut x = *b"<?pi xx";
            *x.last_mut().unwrap() = 0xdd;

            expect(&x).to(fail_parsing_with!(Error::IncompleteXml))
        }
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
                ElementOpenStartComplete,
                AttributeStart(Partial("")),
                AttributeStart(Partial("bbbbbbbbbb")),
                AttributeStart(Complete("")),
                AttributeStartComplete,
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
                    ElementOpenStartComplete,
                    AttributeStart("bbbbbbbbbb"),
                    AttributeStartComplete,
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
            while let Some(t) = self.next_token() {
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
        fn from_ref(token: FusedToken<'_>) -> Self {
            macro_rules! fuse_all_match {
                ($($tt:tt $name:ident,)*) => {
                    match token {
                        $( fuse_all_match!(@pat $tt $name s) => fuse_all_match!(@arm $tt $name s), )*
                    }
                };

                (@pat pass $name:ident $s:ident) => { $name };
                (@arm pass $name:ident $s:ident) => { $name };

                (@pat fuse $name:ident $s:ident) => { $name($s) };
                (@arm fuse $name:ident $s:ident) => { $name($s.to_string()) };

                (@pat stream $name:ident $s:ident) => { $name($s) };
                (@arm stream $name:ident $s:ident) => { $name($s.map(|i| i.to_string())) };
            }

            fuse_invoke!(fuse_all_match)
        }
    }

    impl FuseCore {
        fn fuse_all<'a>(
            tokens: impl IntoIterator<Item = IndexTokenInner<'a>>,
        ) -> super::Result<Vec<FusedOwnedToken>, super::FuseError> {
            let mut collected = vec![];
            let mut me = Self::default();

            for token in tokens {
                collected.extend({
                    let idx = me.push(token);
                    idx.map(FusedOwnedToken::from_ref)
                });
            }

            collected.extend({
                let idx = me.finish()?;
                idx.map(FusedOwnedToken::from_ref)
            });

            Ok(collected)
        }
    }
}
