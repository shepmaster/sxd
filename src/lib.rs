#![deny(rust_2018_idioms)]
#![allow(dead_code)]

use async_trait::async_trait;
use easy_ext::ext;
use itertools::Itertools;
use snafu::Snafu;
use std::{mem, str};

#[async_trait]
trait DataSource {
    async fn read(&mut self, buffer: &mut [u8]) -> usize;
}

#[derive(Debug)]
struct StringRing<S> {
    buffer: Vec<u8>,
    n_offset_bytes: usize,
    n_utf8_bytes: usize,
    n_dangling_bytes: usize,

    source: S,
}

impl<S> StringRing<S>
where
    S: DataSource,
{
    fn as_str(&self) -> &str {
        // TODO: Alternate implementation without checks
        let bytes = &self.buffer[self.n_offset_bytes..][..self.n_utf8_bytes];
        str::from_utf8(bytes).expect("Safety invariant failed")
    }

    async fn extend(&mut self) {
        let buffer =
            &mut self.buffer[self.n_offset_bytes..][self.n_utf8_bytes..][self.n_dangling_bytes..];
        assert_ne!(0, buffer.len(), "Need to extend buffer or shuffle data");

        let n_new_bytes = self.source.read(buffer).await;
        self.n_dangling_bytes += n_new_bytes;

        let dangling_bytes =
            &self.buffer[self.n_offset_bytes..][self.n_utf8_bytes..][..self.n_dangling_bytes];
        let n_new_utf8_bytes = match str::from_utf8(dangling_bytes) {
            Ok(s) => s.len(),
            Err(e) => match e.error_len() {
                Some(_) => panic!("Invalid UTF8"),
                None => e.valid_up_to(),
            },
        };

        self.n_dangling_bytes -= n_new_utf8_bytes;
        self.n_utf8_bytes += n_new_utf8_bytes;
    }

    async fn complete(&mut self) -> bool {
        if !self.as_str().is_empty() {
            return false;
        }

        self.extend().await;

        self.as_str().is_empty()
    }

    async fn starts_with(&mut self, needle: &str) -> bool {
        let mut s = self.as_str();

        while s.len() < needle.len() {
            // TODO: Avoid infinite loop
            self.extend().await;
            s = self.as_str();
        }

        s.starts_with(needle)
    }

    fn advance(&mut self, n_bytes: usize) {
        self.n_offset_bytes += n_bytes;
        self.n_utf8_bytes = self.n_utf8_bytes.saturating_sub(n_bytes);
    }

    async fn consume(&mut self, s: &str) -> bool {
        if self.starts_with(s).await {
            self.advance(s.len());
            true
        } else {
            false
        }
    }

    async fn consume_until(&mut self, needle: &str) -> &str {
        let mut s = self.as_str();
        if s.is_empty() {
            self.extend().await;
            s = self.as_str();
        }

        match s.find(needle) {
            Some(x) => &self.as_str()[..x],
            None => unimplemented!(),
        }
    }

    async fn consume_space(&mut self) {
        let mut s = self.as_str();

        while s.is_empty() {
            self.extend().await;
            s = self.as_str();
            // TODO: Avoid infinite loop
        }

        loop {
            let space = s
                .char_indices()
                .peeking_take_while(|(_, c)| c.is_space())
                .last()
                .map(|(i, c)| i + c.len_utf8());

            match space {
                Some(l) => {
                    let is_entire_string = l == s.len();

                    self.advance(l);

                    if !is_entire_string {
                        break;
                    }

                    self.extend().await;
                    s = self.as_str();
                }
                None => break,
            }
        }
    }

    async fn name(&mut self) -> &str {
        let mut s = self.as_str();

        while s.is_empty() {
            self.extend().await;
            s = self.as_str();
            // TODO: Avoid infinite loop
        }

        let mut c = s.char_indices();

        let end_idx = match c
            .next()
            .filter(|(_, c)| c.is_name_start_char())
            .map(|(i, c)| i + c.len_utf8())
        {
            Some(i) => i,
            None => panic!("empty string"),
        };

        let end_idx = c
            .take_while(|(_, c)| c.is_name_char())
            .map(|(i, c)| i + c.len_utf8())
            .last()
            .unwrap_or(end_idx);

        &self.as_str()[..end_idx]
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
    AfterElementName,
    AfterAttributeName,
    AfterAttributeValue,
    AfterElementClosed,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Token<'a> {
    ElementStart(&'a str),
    ElementSelfClose,

    AttributeName(&'a str),
    AttributeValue(&'a str),
}

#[async_trait(?Send)]
trait TokenSource {
    async fn next(&mut self) -> Option<Result<Token<'_>>>;
}

#[derive(Debug)]
struct Parser<S> {
    buffer: StringRing<S>,
    state: State,
    to_advance: usize,
}

#[async_trait(?Send)]
impl<S> TokenSource for Parser<S>
where
    S: DataSource,
{
    async fn next(&mut self) -> Option<Result<Token<'_>>> {
        use {State::*, Token::*};

        let to_advance = mem::take(&mut self.to_advance);
        self.buffer.advance(to_advance);

        match self.state {
            Initial => {
                if self.buffer.consume("<").await {
                    // space?
                    let name = self.buffer.name().await;

                    self.state = AfterElementName;
                    self.to_advance = name.len();
                    Some(Ok(ElementStart(name)))
                } else {
                    panic!()
                }
            }

            AfterElementName => {
                self.buffer.consume_space().await;

                if self.buffer.consume("/>").await {
                    self.state = AfterElementClosed;
                    Some(Ok(ElementSelfClose))
                } else {
                    let name = self.buffer.name().await;

                    self.state = AfterAttributeName;
                    self.to_advance = name.len();
                    Some(Ok(AttributeName(name)))
                }
            }

            AfterAttributeName => {
                self.buffer.consume_space().await;
                self.buffer.consume("=").await;
                self.buffer.consume_space().await;

                self.buffer.consume("\"").await;
                let value = self.buffer.consume_until("\"").await;

                self.state = AfterAttributeValue;
                self.to_advance = value.len();
                Some(Ok(AttributeValue(value)))
            }

            AfterElementClosed => {
                if self.buffer.complete().await {
                    return None;
                }

                unimplemented!()
            }

            s => unimplemented!("state: {:?}", s),
        }
    }
}

#[derive(Debug, Snafu)]
enum Error {}

type Result<T, E = Error> = std::result::Result<T, E>;

#[cfg(test)]
mod test {
    use easy_ext::ext;
    use futures::executor::block_on;

    use super::*;

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

    #[test]
    fn self_closed_element() -> Result {
        block_on(async {
            let src = CompleteSource {
                data: b"<alpha />".to_vec(),
                offset: 0,
            };
            let buffer = StringRing {
                buffer: vec![0; 1024],
                source: src,
                n_offset_bytes: 0,
                n_utf8_bytes: 0,
                n_dangling_bytes: 0,
            };
            let mut parser = Parser {
                buffer,
                state: State::Initial,
                to_advance: 0,
            };

            let tokens = parser.collect_owned().await?;

            use Token::*;
            assert_eq!(tokens, [ElementStart("alpha"), ElementSelfClose]);

            Ok(())
        })
    }

    /// Always reads as much data as possible
    struct CompleteSource {
        data: Vec<u8>,
        offset: usize,
    }

    #[async_trait]
    impl DataSource for CompleteSource {
        async fn read(&mut self, buffer: &mut [u8]) -> usize {
            let remaining = &self.data[self.offset..];
            let len = std::cmp::min(remaining.len(), buffer.len());

            let remaining = &remaining[..len];
            let buffer = &mut buffer[..len];

            buffer.copy_from_slice(remaining);
            self.offset += len;
            len
        }
    }

    /// Owns all the string data to avoid keeping references alive
    #[derive(Debug, Clone, PartialEq, Eq)]
    enum OwnedToken {
        ElementStart(String),
        ElementSelfClose,
        AttributeName(String),
        AttributeValue(String),
    }

    impl From<Token<'_>> for OwnedToken {
        fn from(other: Token<'_>) -> Self {
            match other {
                Token::ElementStart(s) => Self::ElementStart(s.to_owned()),
                Token::ElementSelfClose => Self::ElementSelfClose,
                Token::AttributeName(s) => Self::AttributeName(s.to_owned()),
                Token::AttributeValue(s) => Self::AttributeValue(s.to_owned()),
            }
        }
    }

    impl PartialEq<Token<'_>> for OwnedToken {
        fn eq(&self, other: &Token<'_>) -> bool {
            match (self, other) {
                (Self::ElementStart(s1), Token::ElementStart(s2)) => s1 == s2,
                (Self::ElementSelfClose, Token::ElementSelfClose) => true,
                _ => false,
            }
        }
    }

    #[ext]
    #[async_trait(?Send)]
    impl<T> T
    where
        Self: TokenSource,
    {
        async fn collect_owned(&mut self) -> Result<Vec<OwnedToken>> {
            let mut v = vec![];
            while let Some(t) = self.next().await {
                v.push(t?.into());
            }
            Ok(v)
        }
    }
}
