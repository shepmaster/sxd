use once_cell::sync::Lazy;
use pull_parser::{Fuse, Parser};
use regex::Regex;
use snafu::{ensure, OptionExt, Snafu};
use std::{collections::HashSet, io::Read};
use string_slab::{CheckedArena, CheckedKey};
use token::Token;

#[derive(Debug, Default)]
struct ValidatorCore {
    arena: CheckedArena,
    element_stack: Vec<CheckedKey>,
    attributes: HashSet<CheckedKey>,
    count: usize,
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
        } = self;

        if *count == 0 {
            ensure!(
                matches!(token, DeclarationStart(_) | ElementOpenStart(_)),
                MustStartWithDeclarationOrElement,
            );
        }

        *count += 1;

        match &token {
            // TODO: validate reference values are in-bounds
            DeclarationStart(v) => {
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

            AttributeName(v) => {
                let v = v.as_ref();

                ensure!(!v.is_empty(), AttributeNameEmpty);

                ensure!(
                    attributes.insert(arena.intern(v)),
                    AttributeDuplicate { name: v },
                )
            }

            CharData(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty(),
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
            }
            ReferenceHex(v) => {
                let v = v.as_ref();
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceHexOutsideOfElement { text: v }
                );
            }

            ProcessingInstructionStart(v) => {
                let v = v.as_ref();
                ensure!(!v.is_empty(), ProcessingInstructionNameEmpty);
            }

            _ => {}
        };

        Ok(token)
    }

    fn finish(&mut self) -> Result<()> {
        let Self {
            arena,
            element_stack,
            ..
        } = self;

        if let Some(opened) = element_stack.pop() {
            let name = &arena[opened];
            return ElementOpenedWithoutClose { name }.fail();
        }

        Ok(())
    }
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
    MustStartWithDeclarationOrElement,

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

    AttributeDuplicate {
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

    #[macro_export]
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
    fn fail_unknown_declaration_version() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.a")]);

        assert_error!(&e, Error::InvalidDeclarationVersion { version } if version == "1.a");
    }

    #[test]
    fn fail_char_data_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), CharData("a")]);

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
    fn fail_element_name_empty() {
        let e = ValidatorCore::validate_all(vec![ElementOpenStart("")]);

        assert_error!(&e, Error::ElementNameEmpty);
    }

    #[test]
    fn fail_attribute_name_empty() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeName(""),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeNameEmpty);
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
    fn fail_duplicated_attribute_name() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeName("b"),
            AttributeValue("c"),
            AttributeName("b"),
            AttributeValue("d"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeDuplicate { name } if name == "b");
    }

    #[test]
    fn fail_does_not_start_with_declaration_or_element() {
        let e = ValidatorCore::validate_all(vec![ReferenceNamed("lt")]);

        assert_error!(&e, Error::MustStartWithDeclarationOrElement);
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
