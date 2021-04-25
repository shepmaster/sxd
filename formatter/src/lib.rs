use std::{fmt, io::Write};
use token::{Streaming, Token};

pub struct Formatter<W> {
    output: W,

    quote: &'static str,

    // Use bitflags instead?
    inside_declaration_start: bool,
    inside_element_open_start: bool,
    inside_element_close: bool,
    inside_attribute_start: bool,
    inside_attribute_value_reference_named: bool,
    inside_attribute_value_reference_decimal: bool,
    inside_attribute_value_reference_hex: bool,
    inside_reference_named: bool,
    inside_reference_decimal: bool,
    inside_reference_hex: bool,
    inside_processing_instruction_start: bool,
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
            inside_element_open_start: false,
            inside_element_close: false,
            inside_attribute_start: false,
            inside_attribute_value_reference_named: false,
            inside_attribute_value_reference_decimal: false,
            inside_attribute_value_reference_hex: false,
            inside_reference_named: false,
            inside_reference_decimal: false,
            inside_reference_hex: false,
            inside_processing_instruction_start: false,
            inside_comment: false,
        }
    }

    pub fn write_token<V>(&mut self, token: Token<Streaming<V>>) -> std::io::Result<()>
    where
        Streaming<V>: fmt::Debug + fmt::Display,
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
            DeclarationClose => write!(output, "?>"),
            ElementOpenStart(v) => pre_post!(inside_element_open_start, ("<"), v,),
            ElementOpenEnd => write!(output, ">"),
            ElementSelfClose => write!(output, "/>"),
            ElementClose(v) => pre_post!(inside_element_close, ("</"), v, (">")),
            AttributeStart(v) => pre_post!(inside_attribute_start, (" "), v, ("={}", quote)),
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
            ProcessingInstructionValue(v) => write!(output, "{}", v),
            ProcessingInstructionEnd => write!(output, "?>"),
            Comment(v) => pre_post!(inside_comment, ("<!--"), v, ("-->")),
        }
    }
}

#[cfg(test)]
mod test {
    use token::{Streaming::*, Token::*};

    use super::*;

    type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

    #[test]
    fn declaration_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(DeclarationStart(Partial("1.")))?;
        f.write_token(DeclarationStart(Partial("23")))?;
        f.write_token(DeclarationStart(Complete("45")))?;

        assert_eq!(out, br#"<?xml version="1.2345""#);

        Ok(())
    }

    #[test]
    fn element_open_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ElementOpenStart(Partial("ab")))?;
        f.write_token(ElementOpenStart(Partial("cd")))?;
        f.write_token(ElementOpenStart(Complete("ef")))?;

        assert_eq!(out, br#"<abcdef"#);

        Ok(())
    }

    #[test]
    fn element_close() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ElementClose(Partial("ab")))?;
        f.write_token(ElementClose(Partial("cd")))?;
        f.write_token(ElementClose(Complete("ef")))?;

        assert_eq!(out, br#"</abcdef>"#);

        Ok(())
    }

    #[test]
    fn attribute_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(AttributeStart(Partial("ab")))?;
        f.write_token(AttributeStart(Partial("cd")))?;
        f.write_token(AttributeStart(Complete("ef")))?;

        assert_eq!(out, br#" abcdef=""#);

        Ok(())
    }

    #[test]
    fn attribute_value_literal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(AttributeValueLiteral(Partial("ab")))?;
        f.write_token(AttributeValueLiteral(Partial("cd")))?;
        f.write_token(AttributeValueLiteral(Complete("ef")))?;

        assert_eq!(out, br#"abcdef"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_named() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(AttributeValueReferenceNamed(Partial("ab")))?;
        f.write_token(AttributeValueReferenceNamed(Partial("cd")))?;
        f.write_token(AttributeValueReferenceNamed(Complete("ef")))?;

        assert_eq!(out, br#"&abcdef;"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_decimal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(AttributeValueReferenceDecimal(Partial("12")))?;
        f.write_token(AttributeValueReferenceDecimal(Partial("34")))?;
        f.write_token(AttributeValueReferenceDecimal(Complete("56")))?;

        assert_eq!(out, br#"&#123456;"#);

        Ok(())
    }

    #[test]
    fn attribute_value_reference_hex() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(AttributeValueReferenceHex(Partial("AB")))?;
        f.write_token(AttributeValueReferenceHex(Partial("CD")))?;
        f.write_token(AttributeValueReferenceHex(Complete("EF")))?;

        assert_eq!(out, br#"&#xABCDEF;"#);

        Ok(())
    }

    #[test]
    fn reference_named() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ReferenceNamed(Partial("ab")))?;
        f.write_token(ReferenceNamed(Partial("cd")))?;
        f.write_token(ReferenceNamed(Complete("ef")))?;

        assert_eq!(out, br#"&abcdef;"#);

        Ok(())
    }

    #[test]
    fn reference_decimal() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ReferenceDecimal(Partial("12")))?;
        f.write_token(ReferenceDecimal(Partial("34")))?;
        f.write_token(ReferenceDecimal(Complete("56")))?;

        assert_eq!(out, br#"&#123456;"#);

        Ok(())
    }

    #[test]
    fn reference_hex() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ReferenceHex(Partial("AB")))?;
        f.write_token(ReferenceHex(Partial("CD")))?;
        f.write_token(ReferenceHex(Complete("EF")))?;

        assert_eq!(out, br#"&#xABCDEF;"#);

        Ok(())
    }

    #[test]
    fn cdata() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(CData(Partial("ab")))?;
        f.write_token(CData(Partial("cd")))?;
        f.write_token(CData(Complete("ef")))?;

        assert_eq!(out, br#"<![CDATA[abcdef]]>"#);

        Ok(())
    }

    #[test]
    fn processing_instruction_start() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ProcessingInstructionStart(Partial("ab")))?;
        f.write_token(ProcessingInstructionStart(Partial("cd")))?;
        f.write_token(ProcessingInstructionStart(Complete("ef")))?;

        assert_eq!(out, br#"<?abcdef"#);

        Ok(())
    }

    #[test]
    fn processing_instruction_value() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(ProcessingInstructionValue(Partial("ab")))?;
        f.write_token(ProcessingInstructionValue(Partial("cd")))?;
        f.write_token(ProcessingInstructionValue(Complete("ef")))?;

        assert_eq!(out, br#"abcdef"#);

        Ok(())
    }

    #[test]
    fn comment() -> Result {
        let mut out = vec![];
        let mut f = Formatter::new(&mut out);

        f.write_token(Comment(Partial("ab")))?;
        f.write_token(Comment(Partial("cd")))?;
        f.write_token(Comment(Complete("ef")))?;

        assert_eq!(out, br#"<!--abcdef-->"#);

        Ok(())
    }
}
