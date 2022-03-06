#![deny(rust_2018_idioms)]

use std::{fmt, io::Write};
use token::{IsComplete, Token, TokenKind};

pub trait Format: IsComplete + fmt::Debug + fmt::Display {}

impl<T> Format for T where T: IsComplete + fmt::Debug + fmt::Display {}

pub trait FormatKind:
    TokenKind<
    DeclarationStart = Self::FmtDeclarationStart,
    DeclarationEncoding = Self::FmtDeclarationEncoding,
    DeclarationStandalone = Self::FmtDeclarationStandalone,
    ElementOpenStart = Self::FmtElementOpenStart,
    ElementOpenStartSuffix = Self::FmtElementOpenStartSuffix,
    ElementClose = Self::FmtElementClose,
    ElementCloseSuffix = Self::FmtElementCloseSuffix,
    AttributeStart = Self::FmtAttributeStart,
    AttributeStartSuffix = Self::FmtAttributeStartSuffix,
    AttributeValueLiteral = Self::FmtAttributeValueLiteral,
    AttributeValueReferenceNamed = Self::FmtAttributeValueReferenceNamed,
    AttributeValueReferenceDecimal = Self::FmtAttributeValueReferenceDecimal,
    AttributeValueReferenceHex = Self::FmtAttributeValueReferenceHex,
    CharData = Self::FmtCharData,
    CData = Self::FmtCData,
    ReferenceNamed = Self::FmtReferenceNamed,
    ReferenceDecimal = Self::FmtReferenceDecimal,
    ReferenceHex = Self::FmtReferenceHex,
    ProcessingInstructionStart = Self::FmtProcessingInstructionStart,
    ProcessingInstructionValue = Self::FmtProcessingInstructionValue,
    Comment = Self::FmtComment,
>
{
    type FmtDeclarationStart: Format;
    type FmtDeclarationEncoding: Format;
    type FmtDeclarationStandalone: Format;
    type FmtElementOpenStart: Format;
    type FmtElementOpenStartSuffix: Format;
    type FmtElementClose: Format;
    type FmtElementCloseSuffix: Format;
    type FmtAttributeStart: Format;
    type FmtAttributeStartSuffix: Format;
    type FmtAttributeValueLiteral: Format;
    type FmtAttributeValueReferenceNamed: Format;
    type FmtAttributeValueReferenceDecimal: Format;
    type FmtAttributeValueReferenceHex: Format;
    type FmtCharData: Format;
    type FmtCData: Format;
    type FmtReferenceNamed: Format;
    type FmtReferenceDecimal: Format;
    type FmtReferenceHex: Format;
    type FmtProcessingInstructionStart: Format;
    type FmtProcessingInstructionValue: Format;
    type FmtComment: Format;
}

impl<K> FormatKind for K
where
    K: TokenKind,
    Self::DeclarationStart: Format,
    Self::DeclarationEncoding: Format,
    Self::DeclarationStandalone: Format,
    Self::ElementOpenStart: Format,
    Self::ElementOpenStartSuffix: Format,
    Self::ElementClose: Format,
    Self::ElementCloseSuffix: Format,
    Self::AttributeStart: Format,
    Self::AttributeStartSuffix: Format,
    Self::AttributeValueLiteral: Format,
    Self::AttributeValueReferenceNamed: Format,
    Self::AttributeValueReferenceDecimal: Format,
    Self::AttributeValueReferenceHex: Format,
    Self::CharData: Format,
    Self::CData: Format,
    Self::ReferenceNamed: Format,
    Self::ReferenceDecimal: Format,
    Self::ReferenceHex: Format,
    Self::ProcessingInstructionStart: Format,
    Self::ProcessingInstructionValue: Format,
    Self::Comment: Format,
{
    type FmtDeclarationStart = Self::DeclarationStart;
    type FmtDeclarationEncoding = Self::DeclarationEncoding;
    type FmtDeclarationStandalone = Self::DeclarationStandalone;
    type FmtElementOpenStart = Self::ElementOpenStart;
    type FmtElementOpenStartSuffix = Self::ElementOpenStartSuffix;
    type FmtElementClose = Self::ElementClose;
    type FmtElementCloseSuffix = Self::ElementCloseSuffix;
    type FmtAttributeStart = Self::AttributeStart;
    type FmtAttributeStartSuffix = Self::AttributeStartSuffix;
    type FmtAttributeValueLiteral = Self::AttributeValueLiteral;
    type FmtAttributeValueReferenceNamed = Self::AttributeValueReferenceNamed;
    type FmtAttributeValueReferenceDecimal = Self::AttributeValueReferenceDecimal;
    type FmtAttributeValueReferenceHex = Self::AttributeValueReferenceHex;
    type FmtCharData = Self::CharData;
    type FmtCData = Self::CData;
    type FmtReferenceNamed = Self::ReferenceNamed;
    type FmtReferenceDecimal = Self::ReferenceDecimal;
    type FmtReferenceHex = Self::ReferenceHex;
    type FmtProcessingInstructionStart = Self::ProcessingInstructionStart;
    type FmtProcessingInstructionValue = Self::ProcessingInstructionValue;
    type FmtComment = Self::Comment;
}

pub struct Formatter<W> {
    output: W,

    quote: &'static str,

    // Use bitflags instead?
    inside_declaration_start: bool,
    inside_declaration_encoding: bool,
    inside_declaration_standalone: bool,
    inside_element_open_start: bool,
    inside_element_open_start_suffix: bool,
    inside_element_close: bool,
    inside_element_close_suffix: bool,
    inside_attribute_start: bool,
    inside_attribute_start_suffix: bool,
    inside_attribute_value_reference_named: bool,
    inside_attribute_value_reference_decimal: bool,
    inside_attribute_value_reference_hex: bool,
    inside_reference_named: bool,
    inside_reference_decimal: bool,
    inside_reference_hex: bool,
    inside_processing_instruction_start: bool,
    inside_processing_instruction_value: bool,
    inside_comment: bool,
}

impl<W> Formatter<W>
where
    W: Write,
{
    pub fn new(output: W) -> Self {
        Self {
            output,
            quote: r#"""#,
            inside_declaration_start: false,
            inside_declaration_encoding: false,
            inside_declaration_standalone: false,
            inside_element_open_start: false,
            inside_element_open_start_suffix: false,
            inside_element_close: false,
            inside_element_close_suffix: false,
            inside_attribute_start: false,
            inside_attribute_start_suffix: false,
            inside_attribute_value_reference_named: false,
            inside_attribute_value_reference_decimal: false,
            inside_attribute_value_reference_hex: false,
            inside_reference_named: false,
            inside_reference_decimal: false,
            inside_reference_hex: false,
            inside_processing_instruction_start: false,
            inside_processing_instruction_value: false,
            inside_comment: false,
        }
    }

    pub fn write_token<K>(&mut self, token: Token<K>) -> std::io::Result<()>
    where
        K: FormatKind,
    {
        use token::Token::*;

        let Self { output, quote, .. } = self;

        macro_rules! pre_post {
            ($state:ident, $(($($head:tt),*))?, $body:expr, $(($($tail:tt),*))?) => {{
                $(
                    if !self.$state {
                        write!(output, $($head,)*)?;
                    }
                )?

                write!(output, "{}", $body)?;

                self.$state = !$body.is_complete();

                $(
                    if !self.$state {
                        write!(output, $($tail,)*)?;
                    }
                )?

                Ok(())
            }};
        }

        match token {
            DeclarationStart(v) => pre_post!(
                inside_declaration_start,
                ("<?xml version={}", quote),
                v,
                ("{}", quote)
            ),
            DeclarationEncoding(v) => pre_post!(
                inside_declaration_encoding,
                (" encoding={}", quote),
                v,
                ("{}", quote)
            ),
            DeclarationStandalone(v) => pre_post!(
                inside_declaration_standalone,
                (" standalone={}", quote),
                v,
                ("{}", quote)
            ),
            DeclarationClose => write!(output, "?>"),
            ElementOpenStart(v) => pre_post!(inside_element_open_start, ("<"), v,),
            ElementOpenStartSuffix(v) => pre_post!(inside_element_open_start_suffix, (":"), v,),
            ElementOpenStartComplete => Ok(()),
            ElementOpenEnd => write!(output, ">"),
            ElementSelfClose => write!(output, "/>"),
            ElementClose(v) => pre_post!(inside_element_close, ("</"), v,),
            ElementCloseSuffix(v) => pre_post!(inside_element_close_suffix, (":"), v,),
            ElementCloseComplete => write!(output, ">"),
            AttributeStart(v) => pre_post!(inside_attribute_start, (" "), v,),
            AttributeStartSuffix(v) => pre_post!(inside_attribute_start_suffix, (":"), v,),
            AttributeStartComplete => write!(output, "={}", quote),
            AttributeValueLiteral(v) => write!(output, "{}", v),
            AttributeValueReferenceNamed(v) => {
                pre_post!(inside_attribute_value_reference_named, ("&"), v, (";"))
            }
            AttributeValueReferenceDecimal(v) => {
                pre_post!(inside_attribute_value_reference_decimal, ("&#"), v, (";"))
            }
            AttributeValueReferenceHex(v) => {
                pre_post!(inside_attribute_value_reference_hex, ("&#x"), v, (";"))
            }
            AttributeValueEnd => write!(output, "{}", quote),
            CharData(v) => write!(output, "{}", v),
            CData(v) => pre_post!(inside_reference_named, ("<![CDATA["), v, ("]]>")),
            ReferenceNamed(v) => pre_post!(inside_reference_named, ("&"), v, (";")),
            ReferenceDecimal(v) => pre_post!(inside_reference_decimal, ("&#"), v, (";")),
            ReferenceHex(v) => pre_post!(inside_reference_hex, ("&#x"), v, (";")),
            ProcessingInstructionStart(v) => {
                pre_post!(inside_processing_instruction_start, ("<?"), v,)
            }
            ProcessingInstructionValue(v) => {
                pre_post!(inside_processing_instruction_value, (" "), v,)
            }
            ProcessingInstructionEnd => write!(output, "?>"),
            Comment(v) => pre_post!(inside_comment, ("<!--"), v, ("-->")),
        }
    }
}

#[cfg(test)]
mod test {
    use token::{Streaming, Streaming::*, Token::*, UniformToken};

    use super::*;

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

    #[track_caller]
    fn assert_utf8_bytes_eq(left: &[u8], right: &[u8]) {
        use std::str;

        match (str::from_utf8(left), str::from_utf8(right)) {
            (Ok(left), Ok(right)) => assert_eq!(left, right),
            _ => assert_eq!(left, right),
        }
    }

    macro_rules! assert_utf8_bytes_eq {
        ($left:expr, $right:expr) => {{
            assert_utf8_bytes_eq($left.as_ref(), $right.as_ref())
        }};
    }

    #[test]
    fn declaration_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(DeclarationStart(Partial("1.")))?;
        f.write_token_str(DeclarationStart(Partial("23")))?;
        f.write_token_str(DeclarationStart(Complete("45")))?;

        assert_utf8_bytes_eq!(out, br#"<?xml version="1.2345""#);

        Ok(())
    }

    #[test]
    fn declaration_encoding() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(DeclarationEncoding(Partial("U")))?;
        f.write_token_str(DeclarationEncoding(Partial("TF-")))?;
        f.write_token_str(DeclarationEncoding(Complete("8")))?;

        assert_utf8_bytes_eq!(out, br#" encoding="UTF-8""#);

        Ok(())
    }

    #[test]
    fn declaration_standalone() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(DeclarationStandalone(Partial("y")))?;
        f.write_token_str(DeclarationStandalone(Partial("e")))?;
        f.write_token_str(DeclarationStandalone(Complete("s")))?;

        assert_utf8_bytes_eq!(out, br#" standalone="yes""#);

        Ok(())
    }

    #[test]
    fn element_open_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ElementOpenStart(Partial("ab")))?;
        f.write_token_str(ElementOpenStart(Partial("cd")))?;
        f.write_token_str(ElementOpenStart(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"<abcdef"#);

        Ok(())
    }

    #[test]
    fn element_open_start_suffix() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ElementOpenStart(Complete("ns")))?;
        f.write_token_str(ElementOpenStartSuffix(Partial("ab")))?;
        f.write_token_str(ElementOpenStartSuffix(Complete("cd")))?;

        assert_utf8_bytes_eq!(out, br#"<ns:abcd"#);

        Ok(())
    }

    #[test]
    fn element_close() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ElementClose(Partial("ab")))?;
        f.write_token_str(ElementClose(Partial("cd")))?;
        f.write_token_str(ElementClose(Complete("ef")))?;
        f.write_token_str(ElementCloseComplete)?;

        assert_utf8_bytes_eq!(out, br#"</abcdef>"#);

        Ok(())
    }

    #[test]
    fn element_close_suffix() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ElementClose(Complete("ns")))?;
        f.write_token_str(ElementCloseSuffix(Partial("ab")))?;
        f.write_token_str(ElementCloseSuffix(Complete("cd")))?;
        f.write_token_str(ElementCloseComplete)?;

        assert_utf8_bytes_eq!(out, br#"</ns:abcd>"#);

        Ok(())
    }

    #[test]
    fn attribute_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeStart(Partial("ab")))?;
        f.write_token_str(AttributeStart(Partial("cd")))?;
        f.write_token_str(AttributeStart(Complete("ef")))?;
        f.write_token_str(AttributeStartComplete)?;

        assert_utf8_bytes_eq!(out, br#" abcdef=""#);

        Ok(())
    }

    #[test]
    fn attribute_start_suffix() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeStart(Complete("ns")))?;
        f.write_token_str(AttributeStartSuffix(Partial("ab")))?;
        f.write_token_str(AttributeStartSuffix(Complete("cd")))?;
        f.write_token_str(AttributeStartComplete)?;

        assert_utf8_bytes_eq!(out, br#" ns:abcd=""#);

        Ok(())
    }

    #[test]
    fn attribute_value_literal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeValueLiteral(Partial("ab")))?;
        f.write_token_str(AttributeValueLiteral(Partial("cd")))?;
        f.write_token_str(AttributeValueLiteral(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"abcdef"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_named() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeValueReferenceNamed(Partial("ab")))?;
        f.write_token_str(AttributeValueReferenceNamed(Partial("cd")))?;
        f.write_token_str(AttributeValueReferenceNamed(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"&abcdef;"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_decimal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeValueReferenceDecimal(Partial("12")))?;
        f.write_token_str(AttributeValueReferenceDecimal(Partial("34")))?;
        f.write_token_str(AttributeValueReferenceDecimal(Complete("56")))?;

        assert_utf8_bytes_eq!(out, br#"&#123456;"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_hex() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(AttributeValueReferenceHex(Partial("AB")))?;
        f.write_token_str(AttributeValueReferenceHex(Partial("CD")))?;
        f.write_token_str(AttributeValueReferenceHex(Complete("EF")))?;

        assert_utf8_bytes_eq!(out, br#"&#xABCDEF;"#);

        Ok(())
    }

    #[test]
    fn reference_named() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ReferenceNamed(Partial("ab")))?;
        f.write_token_str(ReferenceNamed(Partial("cd")))?;
        f.write_token_str(ReferenceNamed(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"&abcdef;"#);

        Ok(())
    }

    #[test]
    fn reference_decimal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ReferenceDecimal(Partial("12")))?;
        f.write_token_str(ReferenceDecimal(Partial("34")))?;
        f.write_token_str(ReferenceDecimal(Complete("56")))?;

        assert_utf8_bytes_eq!(out, br#"&#123456;"#);

        Ok(())
    }

    #[test]
    fn reference_hex() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ReferenceHex(Partial("AB")))?;
        f.write_token_str(ReferenceHex(Partial("CD")))?;
        f.write_token_str(ReferenceHex(Complete("EF")))?;

        assert_utf8_bytes_eq!(out, br#"&#xABCDEF;"#);

        Ok(())
    }

    #[test]
    fn cdata() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(CData(Partial("ab")))?;
        f.write_token_str(CData(Partial("cd")))?;
        f.write_token_str(CData(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"<![CDATA[abcdef]]>"#);

        Ok(())
    }

    #[test]
    fn processing_instruction_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ProcessingInstructionStart(Partial("ab")))?;
        f.write_token_str(ProcessingInstructionStart(Partial("cd")))?;
        f.write_token_str(ProcessingInstructionStart(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"<?abcdef"#);

        Ok(())
    }

    #[test]
    fn processing_instruction_value() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(ProcessingInstructionValue(Partial("ab")))?;
        f.write_token_str(ProcessingInstructionValue(Partial("cd")))?;
        f.write_token_str(ProcessingInstructionValue(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#" abcdef"#);

        Ok(())
    }

    #[test]
    fn comment() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token_str(Comment(Partial("ab")))?;
        f.write_token_str(Comment(Partial("cd")))?;
        f.write_token_str(Comment(Complete("ef")))?;

        assert_utf8_bytes_eq!(out, br#"<!--abcdef-->"#);

        Ok(())
    }

    impl<W> Formatter<W>
    where
        W: Write,
    {
        pub fn write_token_str(
            &mut self,
            token: UniformToken<Streaming<&str>>,
        ) -> std::io::Result<()> {
            self.write_token(token)
        }
    }
}
