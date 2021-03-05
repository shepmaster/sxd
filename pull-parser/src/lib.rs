#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use easy_ext::ext;
use snafu::{ensure, Snafu};
use std::{io::Read, mem, str};
use token::{Streaming, Token};

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
            // index 0 is always available since min_str(4)
            if x.as_bytes()[0].is_xml_space() {
                self.advance(4);
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

    /// Warning: only ascii character allowed as needle (<= 127)
    fn consume_bytes_until(&mut self, needle: u8) -> Result<Streaming<usize>> {
        let s = abandon!(self.some_str());

        match memchr::memchr(needle, s.as_bytes()) {
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

    fn space(&mut self) -> Result<Option<usize>> {
        let s = abandon!(self.some_str());

        match s.as_bytes().iter().position(|c| !c.is_xml_space()) {
            Some(0) => Ok(None),
            Some(n) => Ok(Some(n)),
            None => Ok(Some(s.len())), // all chars are whitespace
        }
    }

    // Note that space amounts can be unbounded, which means that any
    // use of this should likely occur at the beginning of a state
    // dispatch.
    fn consume_space(&mut self) -> Result<()> {
        let s = self.as_str();

        let n_bytes_space = s
            .as_bytes()
            .iter()
            .position(|c| !c.is_xml_space())
            .unwrap_or_else(|| s.len());

        let all_space = n_bytes_space == s.len();

        self.advance(n_bytes_space);
        ensure!(!all_space, NeedsMoreInputSpace);

        Ok(())
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
        match self {
            '\u{9}'
            | '\u{A}'
            | '\u{D}'
            | '\u{20}'..='\u{D7FF}'
            | '\u{E000}'..='\u{FFFD}'
            | '\u{10000}'..='\u{10FFFF}' => true,
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
    StreamDeclarationVersion(Quote),
    AfterDeclarationVersion,
    AfterDeclarationVersionSpace,

    StreamElementOpenName,
    AfterElementOpenName,
    AfterElementOpenNameSpace,

    StreamAttributeName,
    AfterAttributeName,
    AfterAttributeNameSpace,
    AfterAttributeEquals,
    AfterAttributeEqualsSpace,
    StreamAttributeValue(Quote),

    StreamElementCloseName,
    AfterElementCloseName,
    AfterElementCloseNameSpace,

    StreamReferenceNamed,
    StreamReferenceDecimal,
    StreamReferenceHex,
    AfterReference,

    StreamProcessingInstructionName,
    AfterProcessingInstructionName,
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

type Tok = Token<Streaming<usize>>;
type RollbackState = (usize, usize);

#[derive(Debug)]
pub struct CoreParser {
    buffer: StringRing,
    state: State,
    to_advance: usize,
    rollback_to: RollbackState,
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
    pub fn next(&mut self) -> Option<Result<Token<Streaming<usize>>>> {
        use State::*;

        let to_advance = mem::take(&mut self.to_advance);
        self.buffer.advance(to_advance);

        if self.buffer.complete() {
            return None;
        }

        self.ratchet(self.state);

        let token = match self.state {
            Initial => self.dispatch_initial(),

            AfterDeclarationOpen => self.dispatch_after_declaration_open(),
            AfterDeclarationOpenSpace => self.dispatch_after_declaration_open_space(),
            StreamDeclarationVersion(quote) => self.dispatch_stream_declaration_version(quote),
            AfterDeclarationVersion => self.dispatch_after_declaration_version(),
            AfterDeclarationVersionSpace => self.dispatch_after_declaration_version_space(),

            StreamElementOpenName => {
                self.dispatch_stream_element_open_name(StringRing::name_continuation)
            }
            AfterElementOpenName => self.dispatch_after_element_open_name(),
            AfterElementOpenNameSpace => self.dispatch_after_element_open_name_space(),

            StreamAttributeName => {
                self.dispatch_stream_attribute_name(StringRing::name_continuation)
            }
            AfterAttributeName => self.dispatch_after_attribute_name(),
            AfterAttributeNameSpace => self.dispatch_after_attribute_name_space(),
            AfterAttributeEquals => self.dispatch_after_attribute_equals(),
            AfterAttributeEqualsSpace => self.dispatch_after_attribute_equals_space(),
            StreamAttributeValue(quote) => self.dispatch_stream_attribute_value(quote),

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

    fn dispatch_initial(&mut self) -> Result<Option<Tok>> {
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

        if let Some(l) = self.buffer.space()? {
            self.to_advance = l;
            Ok(Some(Space(Streaming::Complete(l))))
        } else if let Some(v) = self.buffer.first_char_data()? {
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

    fn dispatch_after_declaration_open(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterDeclarationOpenSpace,
            Self::dispatch_after_declaration_open_space,
        )
    }

    fn dispatch_after_declaration_open_space(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer.require("version")?;
        self.buffer.require("=")?;
        let quote = self.buffer.require_quote()?;

        self.ratchet(StreamDeclarationVersion(quote));
        return self.dispatch_stream_declaration_version(quote);
    }

    fn dispatch_stream_declaration_version(&mut self, quote: Quote) -> Result<Option<Tok>> {
        use {State::*, Token::*};

        let value = self.buffer.consume_bytes_until(quote.to_ascii_char())?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterDeclarationVersion);
            self.to_advance += quote.as_ref().len(); // Include the closing quote
        }

        Ok(Some(DeclarationStart(value)))
    }

    fn dispatch_after_declaration_version(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterDeclarationVersionSpace,
            Self::dispatch_after_declaration_version_space,
        )
    }

    fn dispatch_after_declaration_version_space(&mut self) -> Result<Option<Tok>> {
        use {State::*, Token::*};

        self.buffer.require("?>")?;

        self.ratchet(Initial);
        Ok(Some(DeclarationClose))
    }

    fn dispatch_stream_element_open_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<Tok>> {
        self.stream_from_buffer(f, State::AfterElementOpenName, Token::ElementOpenStart)
    }

    fn dispatch_stream_element_close_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<Tok>> {
        self.stream_from_buffer(f, State::AfterElementCloseName, Token::ElementClose)
    }

    fn dispatch_after_element_open_name(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterElementOpenNameSpace,
            Self::dispatch_after_element_open_name_space,
        )
    }

    fn dispatch_after_element_open_name_space(&mut self) -> Result<Option<Tok>> {
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
    ) -> Result<Option<Tok>> {
        self.stream_from_buffer(f, State::AfterAttributeName, Token::AttributeName)
    }

    fn dispatch_after_attribute_name(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterAttributeNameSpace,
            Self::dispatch_after_attribute_name_space,
        )
    }

    fn dispatch_after_attribute_name_space(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer.require("=")?;

        self.ratchet(AfterAttributeEquals);
        self.dispatch_after_attribute_equals()
    }

    fn dispatch_after_attribute_equals(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterAttributeEqualsSpace,
            Self::dispatch_after_attribute_equals_space,
        )
    }

    fn dispatch_after_attribute_equals_space(&mut self) -> Result<Option<Tok>> {
        use State::*;

        let quote = self.buffer.require_quote()?;

        self.ratchet(StreamAttributeValue(quote));
        self.dispatch_stream_attribute_value(quote)
    }

    fn dispatch_stream_attribute_value(&mut self, quote: Quote) -> Result<Option<Tok>> {
        use {State::*, Token::*};

        let value = self.buffer.consume_bytes_until(quote.to_ascii_char())?;

        self.to_advance = *value.unify();

        if value.is_complete() {
            self.ratchet(AfterElementOpenName);
            self.to_advance += quote.as_ref().len() // Include the closing quote
        }

        Ok(Some(AttributeValue(value)))
    }

    fn dispatch_after_element_close_name(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterElementCloseNameSpace,
            Self::dispatch_after_element_close_name_space,
        )
    }

    fn dispatch_after_element_close_name_space(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer.require(">")?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_reference_named(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<Tok>> {
        self.stream_from_buffer(f, State::AfterReference, Token::ReferenceNamed)
    }

    fn dispatch_stream_reference_decimal(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(
            StringRing::reference_decimal,
            State::AfterReference,
            Token::ReferenceDecimal,
        )
    }

    fn dispatch_stream_reference_hex(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(
            StringRing::reference_hex,
            State::AfterReference,
            Token::ReferenceHex,
        )
    }

    fn dispatch_after_reference(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer.require(";")?;
        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_processing_instruction_name(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
    ) -> Result<Option<Tok>> {
        self.stream_from_buffer(
            f,
            State::AfterProcessingInstructionName,
            Token::ProcessingInstructionStart,
        )
    }

    fn dispatch_after_processing_instruction_name(&mut self) -> Result<Option<Tok>> {
        self.consume_space(
            State::AfterProcessingInstructionNameSpace,
            Self::dispatch_after_processing_instruction_name_space,
        )
    }

    fn dispatch_after_processing_instruction_name_space(&mut self) -> Result<Option<Tok>> {
        use {State::*, Token::*};

        if *self.buffer.consume("?>")? {
            self.ratchet(Initial);
            Ok(Some(ProcessingInstructionEnd))
        } else {
            self.ratchet(StreamProcessingInstructionValue);
            self.dispatch_stream_processing_instruction_value()
        }
    }

    fn dispatch_stream_processing_instruction_value(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(
            StringRing::processing_instruction_value,
            State::AfterProcessingInstructionValue,
            Token::ProcessingInstructionValue,
        )
    }

    fn dispatch_after_processing_instruction_value(&mut self) -> Result<Option<Tok>> {
        use {State::*, Token::*};

        self.buffer.require("?>")?;

        self.ratchet(Initial);
        Ok(Some(ProcessingInstructionEnd))
    }

    fn dispatch_stream_char_data(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(StringRing::char_data, State::Initial, Token::CharData)
    }

    fn dispatch_stream_cdata(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(StringRing::cdata, State::AfterCData, Token::CData)
    }

    fn dispatch_after_cdata(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer.require("]]>")?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    fn dispatch_stream_comment(&mut self) -> Result<Option<Tok>> {
        self.stream_from_buffer(StringRing::comment, State::AfterComment, Token::Comment)
    }

    fn dispatch_after_comment(&mut self) -> Result<Option<Tok>> {
        use State::*;

        self.buffer
            .require_or_else("-->", |location| DoubleHyphenInComment { location }.build())?;

        self.ratchet(Initial);
        self.dispatch_initial()
    }

    // ----------

    fn consume_space(
        &mut self,
        next_state: State,
        next_state_fn: impl FnOnce(&mut Self) -> Result<Option<Tok>>,
    ) -> Result<Option<Tok>> {
        self.buffer.consume_space()?;

        self.ratchet(next_state);
        next_state_fn(self)
    }

    #[inline]
    fn stream_from_buffer(
        &mut self,
        f: impl FnOnce(&mut StringRing) -> Result<Streaming<usize>>,
        next_state: State,
        create: impl FnOnce(Streaming<usize>) -> Tok,
    ) -> Result<Option<Tok>> {
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
    NeedsMoreInputSpace,
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

impl Error {
    fn needs_more_input(&self) -> bool {
        matches!(self, Error::NeedsMoreInput | Error::NeedsMoreInputSpace)
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

    pub fn next(&mut self) -> Option<Result<Token<Streaming<&str>>>> {
        let Self {
            parser,
            source,
            exhausted,
        } = self;

        let v = loop {
            match parser.next() {
                None => { /* Get more data */ }
                Some(Err(e)) if e.needs_more_input() => { /* Get more data */ }
                Some(Err(e)) => break Some(Err(e)),
                Some(Ok(v)) => break Some(Ok(v)),
            }

            let n_new_bytes = parser.refill_using(|buf| source.read(buf));

            match n_new_bytes {
                Ok(Ok(0)) if *exhausted => return None,
                Ok(Ok(0)) => {
                    *exhausted = true;
                    continue;
                }
                Ok(Ok(_)) => continue,
                Ok(Err(e)) => panic!("Report this: {}", e),
                Err(e) => return Some(Err(e)),
            }
        };

        v.map(move |t| t.map(move |t| t.map(move |t| t.map(move |t| &parser.as_str()[..t]))))
    }
}

#[derive(Debug, Default)]
struct FuseCore {
    current: Option<Token<String>>,
}

impl FuseCore {
    fn push(&mut self, t: Token<Streaming<&str>>) -> Option<Token<String>> {
        use {Streaming::*, Token::*};

        let Self { current } = self;

        macro_rules! fuse_tokens {
            ($($vname:ident $(($field:ident))?,)*) => {
                match t {
                    $(
                        fuse_tokens!(@pat $vname $($field)?) => fuse_tokens!(@arm $vname $($field)?)
                            ,)*
                }
            };

            (@pat $vname:ident $field:ident) => { $vname($field) };
            (@arm $vname:ident $field:ident) => {
                match $field {
                    Partial(x) => {
                        match current {
                            Some($vname(s)) => {
                                s.push_str(x);
                                None
                            },
                            Some(other) => unreachable!("Was processing {:?} but didn't see complete before seeing {:?}", other, $field),
                            None => {
                                *current = Some($vname(x.to_string()));
                                None
                            }
                        }
                    },
                    Complete(x) => {
                        match current.take() {
                            Some($vname(mut s)) => {
                                s.push_str(x);
                                Some($vname(s))
                            }
                            Some(other) => unreachable!("Was processing {:?} but didn't see complete before seeing {:?}", other, $field),
                            None => Some($vname(x.to_string())),
                        }
                    },
                }
            };

            (@pat $vname:ident) => { $vname };
            (@arm $vname:ident) => { Some($vname) };
        }

        fuse_tokens! {
            DeclarationStart(val),
            DeclarationClose,

            ElementOpenStart(val),
            ElementOpenEnd,
            ElementSelfClose,
            ElementClose(val),

            AttributeName(val),
            AttributeValue(val),

            CharData(val),
            CData(val),
            Space(val),

            ReferenceNamed(val),
            ReferenceDecimal(val),
            ReferenceHex(val),

            ProcessingInstructionStart(val),
            ProcessingInstructionValue(val),
            ProcessingInstructionEnd,

            Comment(val),
        }
    }

    fn finish(&mut self) -> Result<(), FuseError> {
        match self.current.take() {
            Some(_) => Incomplete.fail(),
            None => Ok(()),
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

    pub fn next(&mut self) -> Option<Result<Token<String>, FuseError>> {
        let Self { inner, core } = self;
        while let Some(t) = inner.next() {
            match t {
                Ok(t) => {
                    if let Some(t) = core.push(t) {
                        return Some(Ok(t));
                    }
                }
                Err(e) => return Some(Err(e.into())),
            }
        }

        match core.finish() {
            Ok(()) => None,
            Err(e) => return Some(Err(e)),
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

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

    macro_rules! assert_error {
        ($e:expr, $p:pat $(if $guard:expr)?) => {
            assert!(
                matches!($e, Err($p) $(if $guard)?),
                "Expected {}, but got {:?}",
                stringify!($p),
                $e,
            )
        };
    }

    #[test]
    fn xml_declaration() -> Result {
        let tokens = Parser::new_from_str(r#"<?xml version="1.0"?>"#).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [DeclarationStart(Complete("1.0")), DeclarationClose],
        );

        Ok(())
    }

    #[test]
    fn xml_declaration_small_capacity() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity(r#"<?xml version="1.123456789"?>"#)
            .collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                DeclarationStart(Partial("1")),
                DeclarationStart(Complete(".123456789")),
                DeclarationClose,
            ],
        );

        Ok(())
    }

    #[test]
    fn xml_declaration_single_quoted() -> Result {
        let tokens = Parser::new_from_str(r#"<?xml version='1.0'?>"#).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [DeclarationStart(Complete("1.0")), DeclarationClose],
        );

        Ok(())
    }

    #[test]
    fn self_closed_element() -> Result {
        let tokens = Parser::new_from_str(r#"<alpha />"#).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [ElementOpenStart(Complete("alpha")), ElementSelfClose]
        );

        Ok(())
    }

    #[test]
    fn self_closed_element_small_capacity() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity(r#"<a01234567890123456789 />"#)
            .collect_owned()?;

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
    }

    #[test]
    fn self_closed_element_with_one_attribute() -> Result {
        let tokens = Parser::new_from_str(r#"<alpha a="b"/>"#).collect_owned()?;

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
    }

    #[test]
    fn self_closed_element_with_one_attribute_small_capacity() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity(
            r#"<a01234567890123456789 b01234567890123456789="c01234567890123456789"/>"#,
        )
        .collect_owned()?;

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
    }

    #[test]
    fn attributes_with_both_quote_styles() -> Result {
        let tokens = Parser::new_from_str(r#"<alpha a="b" c='d'/>"#).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ElementOpenStart(Complete("alpha")),
                AttributeName(Complete("a")),
                AttributeValue(Complete("b")),
                AttributeName(Complete("c")),
                AttributeValue(Complete("d")),
                ElementSelfClose,
            ],
        );

        Ok(())
    }

    #[test]
    fn element_with_no_children() -> Result {
        let tokens = Parser::new_from_str(r#"<alpha></alpha>"#).collect_owned()?;

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
    }

    #[test]
    fn element_with_no_children_small_capacity() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity(
            r#"<a01234567890123456789></a01234567890123456789>"#,
        )
        .collect_owned()?;

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
    }

    #[test]
    fn element_with_one_child() -> Result {
        let tokens = Parser::new_from_str(r#"<alpha><beta /></alpha>"#).collect_owned()?;

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
    }

    #[test]
    fn char_data() -> Result {
        let tokens = Parser::new_from_str("<a>b</a>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
                CharData(Complete("b")),
                ElementClose(Complete("a")),
            ],
        );

        Ok(())
    }

    #[test]
    fn char_data_small_capacity() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity(r#"<a>01234567890123456789</a>"#)
            .collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
                CharData(Partial("0123456789012")),
                CharData(Complete("3456789")),
                ElementClose(Complete("a"))
            ],
        );

        Ok(())
    }

    #[test]
    fn char_data_with_close_square_bracket() -> Result {
        let tokens = Parser::new_from_str("<a>b]</a>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
                CharData(Complete("b]")),
                ElementClose(Complete("a")),
            ],
        );

        Ok(())
    }

    #[test]
    fn cdata() -> Result {
        let tokens = Parser::new_from_str("<![CDATA[ hello ]]>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [CData(Complete(" hello "))]);

        Ok(())
    }

    #[test]
    fn cdata_small_buffer() -> Result {
        let tokens = Parser::new_from_str_and_min_capacity("<![CDATA[aaaaaaaaaaaaaaaaaaaa]]>")
            .collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [CData(Partial("aaaaaaa")), CData(Complete("aaaaaaaaaaaaa"))]
        );

        Ok(())
    }

    #[test]
    fn only_space() -> Result {
        let tokens = Parser::new_from_str(" \t\r\n").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [Space(Complete(" \t\r\n"))]);

        Ok(())
    }

    #[test]
    fn leading_and_trailing_whitespace_self_closed() -> Result {
        let tokens = Parser::new_from_str("\t <a/>\r\n").collect_owned()?;

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
    }

    #[test]
    fn leading_and_trailing_whitespace() -> Result {
        let tokens = Parser::new_from_str("\t <a></a>\r\n").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                Space(Complete("\t ")),
                ElementOpenStart(Complete("a")),
                ElementOpenEnd,
                ElementClose(Complete("a")),
                Space(Complete("\r\n")),
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction() -> Result {
        let tokens = Parser::new_from_str("<?a?>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Complete("a")),
                ProcessingInstructionEnd,
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction_small_capacity() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("<?aaaaaaaaaaaaaaaaaaaa?>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Partial("aaaaaaaaaaaaaa")),
                ProcessingInstructionStart(Complete("aaaaaa")),
                ProcessingInstructionEnd,
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction_starts_with_xml() -> Result {
        let tokens = Parser::new_from_str("<?xml-but-not-that?>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Complete("xml-but-not-that")),
                ProcessingInstructionEnd,
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction_with_value() -> Result {
        let tokens = Parser::new_from_str("<?a b?>").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Complete("a")),
                ProcessingInstructionValue(Complete("b")),
                ProcessingInstructionEnd,
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction_with_value_small_buffer() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("<?aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbb?>")
                .collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Partial("aaaaaaaaaaaaaa")),
                ProcessingInstructionStart(Complete("aaaaaa")),
                ProcessingInstructionValue(Partial("bbbbbbbbb")),
                ProcessingInstructionValue(Complete("bbbbbbbbbbb")),
                ProcessingInstructionEnd
            ],
        );

        Ok(())
    }

    #[test]
    fn processing_instruction_unclosed() -> Result {
        let tokens = Parser::new_from_str(r"<?a").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [ProcessingInstructionStart(Partial("a"))]);

        Ok(())
    }

    #[test]
    fn comment() -> Result {
        let tokens = Parser::new_from_str("<!-- hello -->").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [Comment(Complete(" hello "))]);

        Ok(())
    }

    #[test]
    fn comment_small_buffer() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("<!--aaaaaaaaaaaaaaaaaaaa-->").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                Comment(Partial("aaaaaaaaaaaa")),
                Comment(Complete("aaaaaaaa")),
            ]
        );

        Ok(())
    }

    #[test]
    fn comment_unclosed() -> Result {
        let tokens = Parser::new_from_str(r##"<!--hello"##).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [Comment(Partial("hello"))]);

        Ok(())
    }

    #[test]
    fn reference_named() -> Result {
        let tokens = Parser::new_from_str("&lt;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [ReferenceNamed(Complete("lt"))]);

        Ok(())
    }

    #[test]
    fn reference_named_small_buffer() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("&aaaaaaaaaaaaaaaaaaaa;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ReferenceNamed(Partial("aaaaaaaaaaaaaaa")),
                ReferenceNamed(Complete("aaaaa")),
            ]
        );

        Ok(())
    }

    #[test]
    fn reference_decimal() -> Result {
        let tokens = Parser::new_from_str("&#42;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [ReferenceDecimal(Complete("42"))]);

        Ok(())
    }

    #[test]
    fn reference_decimal_small_buffer() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("&#11111111111111111111;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ReferenceDecimal(Partial("11111111111111")),
                ReferenceDecimal(Complete("111111")),
            ]
        );

        Ok(())
    }

    #[test]
    fn reference_hex() -> Result {
        let tokens = Parser::new_from_str("&#xBEEF;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [ReferenceHex(Complete("BEEF"))]);

        Ok(())
    }

    #[test]
    fn reference_hex_small_buffer() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity("&#xaaaaaaaaaaaaaaaaaaaa;").collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ReferenceHex(Partial("aaaaaaaaaaaaa")),
                ReferenceHex(Complete("aaaaaaa")),
            ]
        );

        Ok(())
    }

    // After parsing a name to the end of the buffer, when we start
    // parsing again, we need to allow the first character to be a
    // non-start-char.
    #[test]
    fn names_that_span_blocks_can_continue_with_non_start_chars() -> Result {
        let tokens =
            Parser::new_from_str_and_min_capacity(r#"<a----------------/>"#).collect_owned()?;

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
    }

    #[test]
    fn multi_byte_lookahead_at_end_of_input() -> Result {
        // Parser looked for `<?`, which didn't fit and since
        // we ran out of input, we got an error.
        let input = " ";

        let tokens = Parser::new_from_str_and_min_capacity(input).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(tokens, [Space(Complete(" "))]);

        Ok(())
    }

    #[test]
    fn multi_byte_processing_instruction_lookahead_that_spans_blocks() -> Result {
        // Parser looked for `?>`, which was split across the current
        // buffer and the next.
        let input = "<?a aaaaaaaaaaaaaaaaaaaaaaaaaaa?>";

        let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                ProcessingInstructionStart(Complete("a")),
                ProcessingInstructionValue(Partial("aaaaaaaaaaaaaaaaaaaaaaaaaaa")),
                ProcessingInstructionValue(Complete("")),
                ProcessingInstructionEnd,
            ]
        );

        Ok(())
    }

    #[test]
    fn multi_byte_comment_lookahead_that_spans_blocks_1() -> Result {
        let input = "<!--aaaaaaaaaaaaaaaaaaaaaaaaaaa-->";
        //                         The last byte is ^
        let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                Comment(Partial("aaaaaaaaaaaaaaaaaaaaaaaaaaa")),
                Comment(Complete("")),
            ]
        );

        Ok(())
    }

    #[test]
    fn multi_byte_cdata_lookahead_that_spans_blocks_1() -> Result {
        let input = "<![CDATA[aaaaaaaaaaaaaaaaaaaaaa]]>";
        //      this is the last byte in the buffer ^
        let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [
                CData(Partial("aaaaaaaaaaaaaaaaaaaaaa")),
                CData(Complete("")),
            ]
        );

        Ok(())
    }

    #[test]
    fn multi_byte_cdata_lookahead_that_spans_blocks_2() -> Result {
        let input = "<![CDATA[aaaaaaaaaaaaaaaaaaaaa]]>";
        //      this is the last byte in the buffer ^
        let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

        use {Streaming::*, Token::*};
        assert_eq!(
            tokens,
            [CData(Partial("aaaaaaaaaaaaaaaaaaaaa")), CData(Complete(""))],
        );

        Ok(())
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
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<?xml                  version='1.0' ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3

            let tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [DeclarationStart(Complete("1.0")), DeclarationClose],
            );

            Ok(())
        }

        #[test]
        fn after_declaration_version() -> Result {
            for i in 1..=64 {
                let input = format!("<?xml version='1.0'{}?>", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<?xml version='1.0'                                            ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3

            let tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [DeclarationStart(Complete("1.0")), DeclarationClose],
            );

            Ok(())
        }

        #[test]
        fn after_open_element_name() -> Result {
            for i in 1..=64 {
                let input = format!("<a{}>", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<a                                                             >";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(tokens, [ElementOpenStart(Complete("a")), ElementOpenEnd]);

            Ok(())
        }

        #[test]
        fn after_attribute_name() -> Result {
            for i in 1..=64 {
                let input = format!("<a b{}='c' />", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<a b                           ='c' />";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Complete("a")),
                    AttributeName(Complete("b")),
                    AttributeValue(Complete("c")),
                    ElementSelfClose,
                ],
            );

            Ok(())
        }

        #[test]
        fn after_attribute_equal() -> Result {
            for i in 1..=64 {
                let input = format!("<a b={}'c' />", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<a b=                              'c' />";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ElementOpenStart(Complete("a")),
                    AttributeName(Complete("b")),
                    AttributeValue(Complete("c")),
                    ElementSelfClose,
                ],
            );

            Ok(())
        }

        #[test]
        fn after_close_element() -> Result {
            for i in 1..=64 {
                let input = format!("</a{}>", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "</a                               >";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(tokens, [ElementClose(Complete("a"))]);

            Ok(())
        }

        #[test]
        fn after_processing_instruction_name() -> Result {
            for i in 1..=64 {
                let input = format!("<?a{}?>", " ".repeat(i));
                let _tokens = Parser::new_from_str_and_capacity(&input, 32).collect_owned()?;
            }

            let input = "<?a                               ?>";
            //           0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF
            //           0               1               2               3
            let tokens = Parser::new_from_str_and_capacity(input, 32).collect_owned()?;

            use {Streaming::*, Token::*};
            assert_eq!(
                tokens,
                [
                    ProcessingInstructionStart(Complete("a")),
                    ProcessingInstructionEnd,
                ],
            );

            Ok(())
        }
    }

    #[test]
    fn fail_comments_disallow_double_hyphens() -> Result {
        let error = Parser::new_from_str("<!------>").collect_owned();

        assert_error!(error, Error::DoubleHyphenInComment { location: 4 });

        Ok(())
    }

    #[test]
    fn fail_disallowed_ascii_char() -> Result {
        let mut x = [b'a'; 20];
        x[19] = 0x07;
        let error = Parser::new_from_bytes_and_capacity(&x, 16).collect_owned();

        assert_error!(
            error,
            Error::InvalidChar {
                location: 19,
                length: 1
            }
        );

        Ok(())
    }

    #[test]
    fn fail_disallowed_multibyte_char() -> Result {
        let mut x = [b'a'; 20];
        x[17] = 0xEF;
        x[18] = 0xBF;
        x[19] = 0xBF;
        let error = Parser::new_from_bytes_and_capacity(&x, 16).collect_owned();

        assert_error!(
            error,
            Error::InvalidChar {
                location: 17,
                length: 3
            }
        );

        Ok(())
    }

    #[test]
    fn fail_non_utf_8() -> Result {
        let error = Parser::new_from_bytes(&[b'a', b'b', b'c', 0xFF]).collect_owned();

        assert_error!(
            error,
            Error::InputNotUtf8 {
                location: 3,
                length: 1,
            }
        );

        Ok(())
    }

    mod fuse {
        use super::*;
        use {Streaming::*, Token::*};

        #[test]
        fn combines_split_tokens() -> Result {
            let tokens = FuseCore::fuse_all(vec![
                ElementOpenStart(Partial("aaaaa")),
                ElementOpenStart(Complete("aaaaa")),
                AttributeName(Partial("")),
                AttributeName(Partial("bbbbbbbbbb")),
                AttributeName(Complete("")),
                AttributeValue(Partial("c")),
                AttributeValue(Partial("c")),
                AttributeValue(Complete("c")),
                ElementSelfClose,
            ])?;

            assert_eq!(
                tokens,
                [
                    ElementOpenStart("aaaaaaaaaa"),
                    AttributeName("bbbbbbbbbb"),
                    AttributeValue("ccc"),
                    ElementSelfClose,
                ],
            );

            Ok(())
        }

        #[test]
        fn fail_unfinished_tokens() -> Result {
            let error = FuseCore::fuse_all(vec![ElementOpenStart(Partial("a"))]);

            assert_error!(error, FuseError::Incomplete);

            Ok(())
        }
    }

    impl<'a> Parser<&'a [u8]> {
        fn new_from_str(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::DEFAULT_CAPACITY)
        }

        fn new_from_str_and_min_capacity(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::MINIMUM_CAPACITY)
        }

        fn new_from_str_and_capacity(s: &'a str, capacity: usize) -> Self {
            Self::with_buffer_capacity(s.as_bytes(), capacity)
        }

        fn new_from_bytes(s: &'a [u8]) -> Self {
            Self::new_from_bytes_and_capacity(s, StringRing::DEFAULT_CAPACITY)
        }

        fn new_from_bytes_and_capacity(s: &'a [u8], capacity: usize) -> Self {
            Self::with_buffer_capacity(s, capacity)
        }
    }

    impl<R> Parser<R>
    where
        R: Read,
    {
        fn collect_owned(&mut self) -> super::Result<Vec<Token<Streaming<String>>>> {
            let mut v = vec![];
            while let Some(t) = self.next() {
                v.push(t?.map(|s| s.map(str::to_owned)));
            }
            Ok(v)
        }
    }

    impl FuseCore {
        fn fuse_all<'a>(
            tokens: impl IntoIterator<Item = Token<Streaming<&'a str>>>,
        ) -> super::Result<Vec<Token<String>>, super::FuseError> {
            let mut collected = vec![];
            let mut me = Self::default();

            for token in tokens {
                collected.extend(me.push(token));
            }

            me.finish().map(|()| collected)
        }
    }
}
