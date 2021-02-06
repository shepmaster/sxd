use crate::{DataSource, Result, StringRing, Token};
use async_trait::async_trait;
use futures::executor;
use std::io;

/// Adapts an implementer of [`std::io::Read`].
///
/// This should **not** be used unless you are also using the parser
/// from a completely synchronous context.
#[derive(Debug)]
struct ReadAdapter<R>(R);

#[async_trait(?Send)]
impl<R> DataSource for ReadAdapter<R>
where
    R: io::Read,
{
    async fn read(&mut self, buffer: &mut [u8]) -> usize {
        self.0.read(buffer).expect("TODO")
    }
}

pub trait TokenSource {
    fn next(&mut self) -> Option<Result<Token<'_>>>;
}

pub struct Parser<R>(super::Parser<ReadAdapter<R>>);

impl<R> Parser<R>
where
    R: io::Read,
{
    pub fn new(source: R) -> Self {
        Self::with_buffer_capacity(source, StringRing::<ReadAdapter<R>>::DEFAULT_CAPACITY)
    }

    pub fn with_buffer_capacity(source: R, capacity: usize) -> Self {
        let raw = super::Parser::with_buffer_capacity(ReadAdapter(source), capacity);
        Self(raw)
    }
}

impl<R> TokenSource for Parser<R>
where
    R: io::Read,
{
    fn next(&mut self) -> Option<Result<Token<'_>>> {
        use crate::TokenSource;

        // TODO: Would implementing our own executor improve performance?
        executor::block_on(self.0.next())
    }
}

#[cfg(test)]
mod test {
    use crate::{test::OwnedToken, Error, Streaming, StringRing, Token};
    use easy_ext::ext;

    use super::*;

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

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

    impl<'a> Parser<&'a [u8]> {
        fn new_from_str(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::DEFAULT_CAPACITY)
        }

        fn new_from_str_and_min_capacity(s: &'a str) -> Self {
            Self::new_from_str_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::MINIMUM_CAPACITY)
        }

        fn new_from_str_and_capacity(s: &'a str, capacity: usize) -> Self {
            Parser::with_buffer_capacity(s.as_bytes(), capacity)
        }

        fn new_from_bytes(s: &'a [u8]) -> Self {
            Self::new_from_bytes_and_capacity(s, StringRing::<ReadAdapter<&[u8]>>::DEFAULT_CAPACITY)
        }

        fn new_from_bytes_and_capacity(s: &'a [u8], capacity: usize) -> Self {
            Parser::with_buffer_capacity(s, capacity)
        }
    }

    #[ext]
    impl<T> T
    where
        Self: TokenSource,
    {
        fn collect_owned(&mut self) -> super::Result<Vec<OwnedToken>> {
            let mut v = vec![];
            while let Some(t) = self.next() {
                v.push(t?.into());
            }
            Ok(v)
        }
    }
}
