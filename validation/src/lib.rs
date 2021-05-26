use once_cell::sync::Lazy;
use pull_parser::{Fuse, Parser, XmlCharExt, XmlStrExt};
use regex::Regex;
use snafu::{ensure, OptionExt, ResultExt, Snafu};
use std::{collections::HashSet, io::Read};
use string_slab::{CheckedArena, CheckedKey};
use token::Token;

#[derive(Debug, Default)]
struct ValidatorCore {
    arena: CheckedArena,
    element_stack: Vec<CheckedKey>,
    attributes: HashSet<CheckedKey>,
    count: usize,
    seen_one_element: bool,
}

impl ValidatorCore {
    fn push<S>(&mut self, token: Token<S>) -> Result<Token<S>>
    where
        S: AsRef<str>,
    {
        use token::Token::*;

        let Self {
            arena,
            element_stack,
            attributes,
            count,
            seen_one_element,
        } = self;

        if *count == 0 {
            ensure!(
                matches!(
                    token,
                    DeclarationStart(_)
                        | ElementOpenStart(_)
                        | ProcessingInstructionStart(_)
                        | CharData(_)
                ),
                InvalidStartItem,
            );
        }

        match &token {
            DeclarationStart(v) => {
                if *count != 0 {
                    return DeclarationOnlyAllowedAtStart.fail();
                }

                static VALID_VERSION_STRING: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r#"^1\.[0-9]+$"#).unwrap());

                let v = v.as_ref();

                ensure!(
                    VALID_VERSION_STRING.is_match(v),
                    InvalidDeclarationVersion { version: v }
                );
            }

            ElementOpenStart(v) => {
                let v = v.as_ref();

                ensure!(!v.is_empty(), ElementNameEmpty);

                if element_stack.is_empty() {
                    ensure!(!*seen_one_element, MultipleTopLevelElements { name: v });
                    *seen_one_element = true;
                }

                element_stack.push(arena.intern(v));
                attributes.clear();
            }
            ElementSelfClose => {
                element_stack.pop().context(ElementSelfClosedWithoutOpen)?;
            }
            ElementClose(v) => {
                let v = v.as_ref();
                let v = arena.intern(v);
                let name = element_stack.pop().context(ElementClosedWithoutOpen)?;
                ensure!(
                    name == v,
                    ElementOpenAndCloseMismatched {
                        open: &arena[name],
                        close: &arena[v],
                    },
                )
            }

            AttributeStart(v) => {
                let v = v.as_ref();

                ensure!(!v.is_empty(), AttributeNameEmpty);

                ensure!(
                    attributes.insert(arena.intern(v)),
                    AttributeDuplicate { name: v },
                )
            }
            AttributeValueReferenceDecimal(v) => {
                let v = v.as_ref();
                // TODO: Avoid performing this transformation at multiple layers
                reference_value(v, 10).context(InvalidAttributeValueReferenceDecimal)?;
            }
            AttributeValueReferenceHex(v) => {
                let v = v.as_ref();
                // TODO: Avoid performing this transformation at multiple layers
                reference_value(v, 16).context(InvalidAttributeValueReferenceHex)?;
            }

            CharData(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty() || v.is_xml_space(),
                    CharDataOutsideOfElement { text: v }
                );
            }
            CData(v) => {
                let v = v.as_ref();
                ensure!(!element_stack.is_empty(), CDataOutsideOfElement { text: v });
            }

            ReferenceNamed(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceNamedOutsideOfElement { text: v }
                );
            }
            ReferenceDecimal(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceDecimalOutsideOfElement { text: v }
                );
                // TODO: Avoid performing this transformation at multiple layers
                reference_value(v, 10).context(InvalidReferenceDecimal)?;
            }
            ReferenceHex(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceHexOutsideOfElement { text: v }
                );
                // TODO: Avoid performing this transformation at multiple layers
                reference_value(v, 16).context(InvalidReferenceHex)?;
            }

            ProcessingInstructionStart(v) => {
                let v = v.as_ref();
                ensure!(!v.is_empty(), ProcessingInstructionNameEmpty);
                ensure!(
                    !v.eq_ignore_ascii_case("xml"),
                    ProcessingInstructionInvalidName { name: v }
                );
            }

            _ => {}
        };

        *count += 1;

        Ok(token)
    }

    fn finish(&mut self) -> Result<()> {
        let Self {
            arena,
            element_stack,
            seen_one_element,
            ..
        } = self;

        if let Some(opened) = element_stack.pop() {
            let name = &arena[opened];
            return ElementOpenedWithoutClose { name }.fail();
        }

        ensure!(*seen_one_element, NoTopLevelElements);

        Ok(())
    }
}

fn reference_value(value: &str, radix: u32) -> Result<char, ReferenceValueError> {
    let value = u32::from_str_radix(value, radix).context(InvalidValue { value })?;
    let value = char::from_u32(value).context(InvalidUnicodeCharacter { value })?;
    ensure!(
        value.is_allowed_xml_char(),
        DisallowedUnicodeCharacter { value }
    );
    Ok(value)
}

#[derive(Debug, Snafu)]
pub enum ReferenceValueError {
    InvalidValue {
        source: std::num::ParseIntError,
        value: String,
    },
    InvalidUnicodeCharacter {
        value: u32,
    },
    DisallowedUnicodeCharacter {
        value: char,
    },
}

#[derive(Debug)]
pub struct Validator<R> {
    parser: Fuse<R>,
    core: ValidatorCore,
}

impl<R> Validator<R>
where
    R: Read,
{
    pub fn new(parser: Parser<R>) -> Self {
        let parser = Fuse::new(parser);

        Self {
            parser,
            core: Default::default(),
        }
    }

    pub fn next(&mut self) -> Option<Result<Token<String>>> {
        let Self { parser, core } = self;

        match parser.next() {
            None => match core.finish() {
                Ok(()) => None,
                Err(e) => Some(Err(e)),
            },
            Some(Ok(v)) => Some(core.push(v)),
            Some(e) => Some(e.map_err(Into::into)),
        }
    }
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("An XML document cannot start with this item"))]
    InvalidStartItem,

    DeclarationOnlyAllowedAtStart,

    InvalidDeclarationVersion {
        version: String,
    },

    CharDataOutsideOfElement {
        text: String,
    },
    CDataOutsideOfElement {
        text: String,
    },
    ReferenceNamedOutsideOfElement {
        text: String,
    },
    ReferenceDecimalOutsideOfElement {
        text: String,
    },
    ReferenceHexOutsideOfElement {
        text: String,
    },

    InvalidAttributeValueReferenceDecimal {
        source: ReferenceValueError,
    },
    InvalidAttributeValueReferenceHex {
        source: ReferenceValueError,
    },
    InvalidReferenceDecimal {
        source: ReferenceValueError,
    },
    InvalidReferenceHex {
        source: ReferenceValueError,
    },

    ElementNameEmpty,
    AttributeNameEmpty,
    ProcessingInstructionNameEmpty,

    ElementOpenedWithoutClose {
        name: String,
    },
    ElementSelfClosedWithoutOpen,
    ElementClosedWithoutOpen,
    ElementOpenAndCloseMismatched {
        open: String,
        close: String,
    },

    NoTopLevelElements,

    MultipleTopLevelElements {
        name: String,
    },

    AttributeDuplicate {
        name: String,
    },

    ProcessingInstructionInvalidName {
        name: String,
    },

    #[snafu(context(false))]
    Fusing {
        source: pull_parser::FuseError,
    },
}
type Result<T, E = Error> = std::result::Result<T, E>;

#[cfg(test)]
mod test {
    use super::*;
    use token::Token::*;

    macro_rules! assert_ok {
        ($e:expr) => {
            assert!(matches!($e, Ok(_)), "Expected Ok(_), but got {:?}", $e,)
        };
    }

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
    fn fail_declaration_not_at_start() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementClose("a"),
            DeclarationStart("1.0"),
        ]);

        assert_error!(&e, Error::DeclarationOnlyAllowedAtStart);
    }

    #[test]
    fn fail_unknown_declaration_version() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.a")]);

        assert_error!(&e, Error::InvalidDeclarationVersion { version } if version == "1.a");
    }

    #[test]
    fn fail_char_data_outside_element_if_not_whitespace() {
        let e = ValidatorCore::validate_all(vec![CharData("a")]);

        assert_error!(&e, Error::CharDataOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_cdata_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), CData("a")]);

        assert_error!(&e, Error::CDataOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_reference_named_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), ReferenceNamed("a")]);

        assert_error!(&e, Error::ReferenceNamedOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_reference_decimal_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), ReferenceDecimal("a")]);

        assert_error!(&e, Error::ReferenceDecimalOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_reference_hex_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), ReferenceHex("a")]);

        assert_error!(&e, Error::ReferenceHexOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_reference_decimal_invalid() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementOpenEnd,
            ReferenceDecimal("1"),
            ElementClose("a"),
        ]);

        assert_error!(&e, Error::InvalidReferenceDecimal { .. });
    }

    #[test]
    fn fail_reference_hex_invalid() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementOpenEnd,
            ReferenceHex("1"),
            ElementClose("a"),
        ]);

        assert_error!(&e, Error::InvalidReferenceHex { .. });
    }

    #[test]
    fn fail_element_name_empty() {
        let e = ValidatorCore::validate_all(vec![ElementOpenStart("")]);

        assert_error!(&e, Error::ElementNameEmpty);
    }

    #[test]
    fn fail_attribute_name_empty() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart(""),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeNameEmpty);
    }

    #[test]
    fn fail_attribute_value_reference_decimal_invalid() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("b"),
            AttributeValueReferenceDecimal("1"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::InvalidAttributeValueReferenceDecimal { .. });
    }

    #[test]
    fn fail_attribute_value_reference_hex_invalid() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("b"),
            AttributeValueReferenceHex("1"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::InvalidAttributeValueReferenceHex { .. });
    }

    #[test]
    fn fail_processing_instruction_name_empty() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ProcessingInstructionStart(""),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::ProcessingInstructionNameEmpty);
    }

    #[test]
    fn fail_mismatched_open_and_close() {
        let e = ValidatorCore::validate_all(vec![ElementOpenStart("a"), ElementClose("b")]);

        assert_error!(&e, Error::ElementOpenAndCloseMismatched { open, close } if open == "a" && close == "b");
    }

    #[test]
    fn fail_unclosed_open() {
        let e = ValidatorCore::validate_all(vec![ElementOpenStart("a")]);

        assert_error!(&e, Error::ElementOpenedWithoutClose { name } if name == "a");
    }

    #[test]
    fn fail_no_top_level_elements() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0")]);

        assert_error!(&e, Error::NoTopLevelElements);
    }

    #[test]
    fn fail_multiple_top_level_elements() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementSelfClose,
            ElementOpenStart("b"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::MultipleTopLevelElements { name } if name == "b");
    }

    #[test]
    fn fail_duplicated_attribute_name() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("b"),
            AttributeValueLiteral("c"),
            AttributeStart("b"),
            AttributeValueLiteral("d"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeDuplicate { name } if name == "b");
    }

    #[test]
    fn fail_not_quite_xml_processing_instruction() {
        let e = ValidatorCore::validate_all(vec![ProcessingInstructionStart("xMl")]);

        assert_error!(&e, Error::ProcessingInstructionInvalidName { name } if name == "xMl");
    }

    #[test]
    fn may_start_with_whitespace() {
        let e = ValidatorCore::validate_all(vec![
            CharData(" \r\n\t"),
            ElementOpenStart("element"),
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn may_start_with_processing_instruction() {
        let e = ValidatorCore::validate_all(vec![
            ProcessingInstructionStart("zml"),
            ElementOpenStart("element"),
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn fail_does_not_start_with_declaration_element_or_processing_instruction() {
        let e = ValidatorCore::validate_all(vec![ReferenceNamed("lt")]);

        assert_error!(&e, Error::InvalidStartItem);
    }

    mod reference_value {
        use super::*;
        use crate::ReferenceValueError;

        #[test]
        fn fail_invalid_u32() {
            let e = reference_value("9999999999", 10);

            assert_error!(&e, ReferenceValueError::InvalidValue { value, .. } if value == "9999999999");
        }

        #[test]
        fn fail_invalid_char() {
            let e = reference_value("FFFFFFFF", 16);

            assert_error!(&e, ReferenceValueError::InvalidUnicodeCharacter { value } if *value == 0xFFFF_FFFF);
        }

        #[test]
        fn fail_disallowed_char() {
            let e = reference_value("1", 10);

            assert_error!(&e, ReferenceValueError::DisallowedUnicodeCharacter { value } if *value == '\u{1}');
        }
    }

    impl ValidatorCore {
        fn validate_all<S>(tokens: impl IntoIterator<Item = Token<S>>) -> super::Result<()>
        where
            S: AsRef<str>,
        {
            let mut me = Self::default();
            for token in tokens {
                me.push(token)?;
            }
            me.finish()
        }
    }
}
