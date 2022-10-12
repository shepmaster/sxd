#![deny(rust_2018_idioms)]

use hashbrown::HashSet;
use once_cell::sync::Lazy;
use pull_parser::{Fuse, FusedIndex, Parser, XmlCharExt, XmlStrExt};
use regex::Regex;
use snafu::{ensure, OptionExt, ResultExt, Snafu};
use std::{io::Read, marker::PhantomData};
use string_slab::{CheckedArena, CheckedKey};
use token::{Exchange, Streaming, Token, TokenKind};
use util::{QName, QNameBuilder};

#[derive(Debug, Copy, Clone, PartialEq)]
enum XmlNsKind {
    /// `xmlns="..."`
    Default,
    /// `xmlns:foo="..."`
    Named,
}

#[derive(Debug, Default)]
struct ValidatorCore {
    arena: CheckedArena,
    element_stack: Vec<QName<CheckedKey>>,
    attributes: HashSet<QName<CheckedKey>>,
    count: usize,
    seen_one_element: bool,
    pending_element_open_start_name: Option<QNameBuilder<CheckedKey>>,
    pending_element_close_name: Option<QNameBuilder<CheckedKey>>,
    pending_attribute_name: Option<QNameBuilder<CheckedKey>>,
    is_xmlns: Option<XmlNsKind>,
    attribute_value_had_content: bool,
}

impl ValidatorCore {
    fn push<'a, K, E>(&mut self, token: Token<K>, exchanger: &'a E) -> Result<ValidIndexToken<K>>
    where
        K: TokenKind,
        K::DeclarationStart: Copy,
        K::DeclarationEncoding: Copy,
        K::DeclarationStandalone: Copy,
        K::ElementOpenStart: Copy,
        K::ElementOpenStartSuffix: Copy,
        K::ElementClose: Copy,
        K::ElementCloseSuffix: Copy,
        K::AttributeStart: Copy,
        K::AttributeStartSuffix: Copy,
        K::AttributeValueLiteral: Copy,
        K::AttributeValueReferenceNamed: Copy,
        K::AttributeValueReferenceDecimal: Copy,
        K::AttributeValueReferenceHex: Copy,
        K::ReferenceNamed: Copy,
        K::ReferenceDecimal: Copy,
        K::ReferenceHex: Copy,
        K::ProcessingInstructionStart: Copy,
        K::CharData: Copy,
        K::CData: Copy,
        E: Exchange<K::DeclarationStart, Output<'a> = &'a str>,
        E: Exchange<K::DeclarationEncoding, Output<'a> = &'a str>,
        E: Exchange<K::DeclarationStandalone, Output<'a> = &'a str>,
        E: Exchange<K::ElementOpenStart, Output<'a> = &'a str>,
        E: Exchange<K::ElementOpenStartSuffix, Output<'a> = &'a str>,
        E: Exchange<K::ElementClose, Output<'a> = &'a str>,
        E: Exchange<K::ElementCloseSuffix, Output<'a> = &'a str>,
        E: Exchange<K::AttributeStart, Output<'a> = &'a str>,
        E: Exchange<K::AttributeStartSuffix, Output<'a> = &'a str>,
        E: Exchange<K::AttributeValueLiteral, Output<'a> = Streaming<&'a str>>,
        E: Exchange<K::AttributeValueReferenceNamed, Output<'a> = &'a str>,
        E: Exchange<K::AttributeValueReferenceDecimal, Output<'a> = &'a str>,
        E: Exchange<K::AttributeValueReferenceHex, Output<'a> = &'a str>,
        E: Exchange<K::ReferenceNamed, Output<'a> = &'a str>,
        E: Exchange<K::ReferenceDecimal, Output<'a> = &'a str>,
        E: Exchange<K::ReferenceHex, Output<'a> = &'a str>,
        E: Exchange<K::ProcessingInstructionStart, Output<'a> = &'a str>,
        E: Exchange<K::CharData, Output<'a> = Streaming<&'a str>>,
        E: Exchange<K::CData, Output<'a> = Streaming<&'a str>>,
    {
        use token::Token::*;

        if self.count == 0 {
            ensure!(
                matches!(
                    token,
                    DeclarationStart(_)
                        | ElementOpenStart(_)
                        | Comment(_)
                        | ProcessingInstructionStart(_)
                        | CharData(_)
                ),
                InvalidStartItemSnafu,
            );
        }

        if !matches!(token, ElementOpenStartSuffix(..)) {
            self.flush_element_open_start()?;
        }

        if !matches!(token, ElementCloseSuffix(..)) {
            self.flush_element_close()?;
        }

        if !matches!(token, AttributeStartSuffix(..)) {
            self.flush_attribute_start()?;
        }

        let Self {
            arena,
            element_stack,
            pending_element_open_start_name,
            pending_element_close_name,
            pending_attribute_name,
            is_xmlns,
            attribute_value_had_content,
            ..
        } = self;

        let token = match token {
            DeclarationStart(i) => {
                if self.count != 0 {
                    return DeclarationOnlyAllowedAtStartSnafu.fail();
                }

                let v = exchanger.exchange(i);
                ensure!(v == "1.0", InvalidDeclarationVersionSnafu { version: v });

                DeclarationStart(i)
            }

            DeclarationEncoding(i) => {
                static VALID_ENCODING_STRING: Lazy<Regex> =
                    Lazy::new(|| Regex::new(r#"\A[A-Za-z]([A-Za-z0-9._]|'-')*\z"#).unwrap());

                let v = exchanger.exchange(i);

                ensure!(
                    VALID_ENCODING_STRING.is_match(v),
                    InvalidDeclarationEncodingSnafu { encoding: v }
                );

                DeclarationEncoding(i)
            }

            DeclarationStandalone(i) => {
                let v = exchanger.exchange(i);

                ensure!(
                    matches!(v, "yes" | "no"),
                    InvalidDeclarationStandaloneSnafu { standalone: v }
                );

                DeclarationStandalone(i)
            }

            ElementOpenStart(i) => {
                let v = exchanger.exchange(i);

                ensure!(!v.is_empty(), ElementNameEmptySnafu);

                let v = arena.intern(v);
                *pending_element_open_start_name = Some(QNameBuilder::new(v));

                ElementOpenStart(v)
            }

            ElementOpenStartSuffix(i) => {
                let v = exchanger.exchange(i);

                ensure!(!v.is_empty(), ElementNameSuffixEmptySnafu);

                let pending = pending_element_open_start_name
                    .take()
                    .context(ElementNameSuffixWithoutPrefixSnafu)?;

                let local_part = arena.intern(v);
                let name = pending.push(local_part);

                self.finish_element_open_start(name)?;

                ElementOpenStartSuffix(local_part)
            }

            ElementSelfClose => {
                element_stack
                    .pop()
                    .context(ElementSelfClosedWithoutOpenSnafu)?;

                ElementSelfClose
            }

            ElementClose(i) => {
                let v = exchanger.exchange(i);
                let v = arena.intern(v);
                *pending_element_close_name = Some(QNameBuilder::new(v));

                ElementClose(v)
            }

            ElementCloseSuffix(i) => {
                let v = exchanger.exchange(i);

                let pending = pending_element_close_name
                    .take()
                    .context(ElementCloseSuffixWithoutPrefixSnafu)?;

                let local_part = arena.intern(v);
                let name = pending.push(local_part);

                self.finish_element_close(name)?;

                ElementCloseSuffix(local_part)
            }

            AttributeStart(i) => {
                let v = exchanger.exchange(i);

                ensure!(!v.is_empty(), AttributeNameEmptySnafu);

                *is_xmlns = (v == "xmlns").then_some(XmlNsKind::Default);
                *attribute_value_had_content = false;

                let v = arena.intern(v);
                *pending_attribute_name = Some(QNameBuilder::new(v));

                AttributeStart(v)
            }

            AttributeStartSuffix(i) => {
                let v = exchanger.exchange(i);

                ensure!(!v.is_empty(), AttributeNameSuffixEmptySnafu);

                let pending = pending_attribute_name
                    .take()
                    .context(AttributeNameSuffixWithoutPrefixSnafu)?;

                let local_part = arena.intern(v);
                let name = pending.push(local_part);

                if let Some(XmlNsKind::Default) = is_xmlns {
                    *is_xmlns = Some(XmlNsKind::Named);
                }

                self.finish_attribute_start(name)?;

                AttributeStartSuffix(local_part)
            }

            AttributeValueLiteral(i) => {
                let v = *exchanger.exchange(i).unify();
                if !v.is_empty() {
                    *attribute_value_had_content = true;
                }

                AttributeValueLiteral(i)
            }

            AttributeValueReferenceNamed(i) => {
                let v = exchanger.exchange(i);
                ensure!(
                    known_entity(v),
                    AttributeValueReferenceNamedUnknownSnafu { text: v }
                );
                *attribute_value_had_content = true;

                AttributeValueReferenceNamed(i)
            }

            AttributeValueReferenceDecimal(i) => {
                let v = exchanger.exchange(i);
                let v =
                    reference_value(v, 10).context(InvalidAttributeValueReferenceDecimalSnafu)?;
                *attribute_value_had_content = true;

                AttributeValueReferenceDecimal(v)
            }

            AttributeValueReferenceHex(i) => {
                let v = exchanger.exchange(i);
                let v = reference_value(v, 16).context(InvalidAttributeValueReferenceHexSnafu)?;
                *attribute_value_had_content = true;

                AttributeValueReferenceHex(v)
            }

            AttributeValueEnd => {
                let is_xmlns = is_xmlns.take();
                let is_valid = is_xmlns.map_or(true, |v| {
                    v == XmlNsKind::Default || *attribute_value_had_content
                });
                ensure!(is_valid, NamespaceEmptySnafu);

                AttributeValueEnd
            }

            CharData(i) => {
                let v = *exchanger.exchange(i).unify();
                ensure!(
                    !element_stack.is_empty() || v.is_xml_space(),
                    CharDataOutsideOfElementSnafu { text: v }
                );

                CharData(i)
            }

            CData(i) => {
                let v = *exchanger.exchange(i).unify();
                ensure!(
                    !element_stack.is_empty(),
                    CDataOutsideOfElementSnafu { text: v }
                );

                CData(i)
            }

            ReferenceNamed(i) => {
                let v = exchanger.exchange(i);
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceNamedOutsideOfElementSnafu { text: v }
                );
                ensure!(known_entity(v), ReferenceNamedUnknownSnafu { text: v });

                ReferenceNamed(i)
            }

            ReferenceDecimal(i) => {
                let v = exchanger.exchange(i);
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceDecimalOutsideOfElementSnafu { text: v }
                );

                let v = reference_value(v, 10).context(InvalidReferenceDecimalSnafu)?;

                ReferenceDecimal(v)
            }

            ReferenceHex(i) => {
                let v = exchanger.exchange(i);
                ensure!(
                    !element_stack.is_empty(),
                    ReferenceHexOutsideOfElementSnafu { text: v }
                );

                let v = reference_value(v, 16).context(InvalidReferenceHexSnafu)?;

                ReferenceHex(v)
            }

            ProcessingInstructionStart(i) => {
                let v = exchanger.exchange(i);
                ensure!(!v.is_empty(), ProcessingInstructionNameEmptySnafu);
                ensure!(
                    !v.eq_ignore_ascii_case("xml"),
                    ProcessingInstructionInvalidNameSnafu { name: v }
                );

                ProcessingInstructionStart(i)
            }

            // no validations needed
            DeclarationClose => DeclarationClose,
            ElementOpenStartComplete => ElementOpenStartComplete,
            ElementOpenEnd => ElementOpenEnd,
            ElementCloseComplete => ElementCloseComplete,
            AttributeStartComplete => AttributeStartComplete,
            ProcessingInstructionValue(i) => ProcessingInstructionValue(i),
            ProcessingInstructionEnd => ProcessingInstructionEnd,
            Comment(i) => Comment(i),
        };

        self.count += 1;

        Ok(token)
    }

    fn flush_element_open_start(&mut self) -> Result<()> {
        if let Some(name) = self.pending_element_open_start_name.take() {
            let name = name.finish();

            self.finish_element_open_start(name)?;
        }

        Ok(())
    }

    fn finish_element_open_start(&mut self, name: QName<CheckedKey>) -> Result<()> {
        let Self {
            seen_one_element,
            element_stack,
            attributes,
            ..
        } = self;

        if element_stack.is_empty() {
            ensure!(
                !*seen_one_element,
                MultipleTopLevelElementsSnafu {
                    name: self.exchange(name)
                }
            );
            *seen_one_element = true;
        }

        element_stack.push(name);
        attributes.clear();

        Ok(())
    }

    fn flush_element_close(&mut self) -> Result<()> {
        if let Some(name) = self.pending_element_close_name.take() {
            let name = name.finish();

            self.finish_element_close(name)?;
        }

        Ok(())
    }

    fn finish_element_close(&mut self, close: QName<CheckedKey>) -> Result<()> {
        let Self { element_stack, .. } = self;

        let open = element_stack.pop().context(ElementClosedWithoutOpenSnafu)?;
        ensure!(
            open == close,
            ElementOpenAndCloseMismatchedSnafu {
                open: self.exchange(open),
                close: self.exchange(close),
            },
        );

        Ok(())
    }

    fn flush_attribute_start(&mut self) -> Result<()> {
        if let Some(name) = self.pending_attribute_name.take() {
            let name = name.finish();

            self.finish_attribute_start(name)?;
        }

        Ok(())
    }

    fn finish_attribute_start(&mut self, name: QName<CheckedKey>) -> Result<()> {
        let Self { attributes, .. } = self;

        ensure!(
            attributes.insert(name),
            AttributeDuplicateSnafu {
                name: self.exchange(name),
            },
        );

        Ok(())
    }

    fn finish(&mut self) -> Result<()> {
        self.flush_element_open_start()?;
        self.flush_element_close()?;
        self.flush_attribute_start()?;

        let Self {
            element_stack,
            seen_one_element,
            ..
        } = self;

        if let Some(opened) = element_stack.pop() {
            let name = self.exchange(opened);
            return ElementOpenedWithoutCloseSnafu { name }.fail();
        }

        ensure!(*seen_one_element, NoTopLevelElementsSnafu);

        Ok(())
    }
}

impl Exchange<QName<CheckedKey>> for ValidatorCore {
    type Output<'a> = QName<&'a str>;

    fn exchange(&self, idx: QName<CheckedKey>) -> Self::Output<'_> {
        idx.map(|i| &self.arena[i])
    }
}

fn known_entity(entity: &str) -> bool {
    matches!(entity, "lt" | "gt" | "amp" | "quot" | "apos")
}

fn reference_value(value: &str, radix: u32) -> Result<char, ReferenceValueError> {
    let value = u32::from_str_radix(value, radix).context(InvalidValueSnafu { value })?;
    let value = char::from_u32(value).context(InvalidUnicodeCharacterSnafu { value })?;
    ensure!(
        value.is_allowed_xml_char(),
        DisallowedUnicodeCharacterSnafu { value }
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
}

impl<R: Read> token::Source for Validator<R> {
    type IndexKind = ValidIndexKind;
    type StrKind<'a> = ValidKind<'a>
    where
        Self: 'a;
    type Error = Error;

    fn next_index(&mut self) -> Option<Result<ValidIndexToken>> {
        let Self { parser, core } = self;

        match parser.next_index() {
            None => match core.finish() {
                Ok(()) => None,
                Err(e) => Some(Err(e)),
            },
            Some(Ok(v)) => match core.push(v, parser) {
                Ok(v) => Some(Ok(v)),
                Err(e) => Some(Err(e)),
            },
            Some(Err(e)) => Some(Err(e.into())),
        }
    }

    fn next_str(&mut self) -> Option<Result<ValidToken<'_>>> {
        self.next_index()
            .map(|r| r.map(|t| t.exchange_through(self)))
    }
}

impl<R> Validator<R> {
    pub fn arena(&self) -> &CheckedArena {
        &self.core.arena
    }

    pub fn into_arena(self) -> CheckedArena {
        self.core.arena
    }
}

impl<R> token::Exchange<CheckedKey> for Validator<R> {
    type Output<'a> = &'a str
    where
        R: 'a;

    fn exchange(&self, idx: CheckedKey) -> Self::Output<'_> {
        &self.core.arena[idx]
    }
}

impl<R> token::Exchange<char> for Validator<R> {
    type Output<'a> = char
    where
        R: 'a;

    fn exchange(&self, idx: char) -> Self::Output<'_> {
        idx
    }
}

impl<R> token::Exchange<FusedIndex> for Validator<R> {
    type Output<'a> = &'a str
    where
        R: 'a;

    fn exchange(&self, idx: FusedIndex) -> Self::Output<'_> {
        self.parser.exchange(idx)
    }
}

impl<R> token::Exchange<usize> for Validator<R> {
    type Output<'a> = &'a str
    where
        R: 'a;

    fn exchange(&self, idx: usize) -> Self::Output<'_> {
        self.parser.exchange(idx)
    }
}

impl<R> token::Exchange<Streaming<usize>> for Validator<R> {
    type Output<'a> = Streaming<&'a str>
    where
        R: 'a;

    fn exchange(&self, idx: Streaming<usize>) -> Self::Output<'_> {
        self.parser.exchange(idx)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ValidIndexKind<K = pull_parser::FusedIndexKind>(K)
where
    K: TokenKind;

impl<K: TokenKind> TokenKind for ValidIndexKind<K> {
    type DeclarationStart = K::DeclarationStart;
    type DeclarationEncoding = K::DeclarationEncoding;
    type DeclarationStandalone = K::DeclarationStandalone;
    type ElementOpenStart = CheckedKey;
    type ElementOpenStartSuffix = CheckedKey;
    type ElementClose = CheckedKey;
    type ElementCloseSuffix = CheckedKey;
    type AttributeStart = CheckedKey;
    type AttributeStartSuffix = CheckedKey;
    type AttributeValueLiteral = K::AttributeValueLiteral;
    type AttributeValueReferenceNamed = K::AttributeValueReferenceNamed;
    type AttributeValueReferenceDecimal = char;
    type AttributeValueReferenceHex = char;
    type CharData = K::CharData;
    type CData = K::CData;
    type ReferenceNamed = K::ReferenceNamed;
    type ReferenceDecimal = char;
    type ReferenceHex = char;
    type ProcessingInstructionStart = K::ProcessingInstructionStart;
    type ProcessingInstructionValue = K::ProcessingInstructionValue;
    type Comment = K::Comment;
}

pub type ValidIndexToken<K = pull_parser::FusedIndexKind> = Token<ValidIndexKind<K>>;

#[derive(Debug, Copy, Clone)]
pub struct ValidKind<'a, K = pull_parser::FusedKind<'a>>(PhantomData<&'a str>, K)
where
    K: TokenKind;

impl<'a, K: TokenKind> TokenKind for ValidKind<'a, K> {
    type DeclarationStart = K::DeclarationStart;
    type DeclarationEncoding = K::DeclarationEncoding;
    type DeclarationStandalone = K::DeclarationStandalone;
    type ElementOpenStart = &'a str;
    type ElementOpenStartSuffix = &'a str;
    type ElementClose = &'a str;
    type ElementCloseSuffix = &'a str;
    type AttributeStart = &'a str;
    type AttributeStartSuffix = &'a str;
    type AttributeValueLiteral = K::AttributeValueLiteral;
    type AttributeValueReferenceNamed = K::AttributeValueReferenceNamed;
    type AttributeValueReferenceDecimal = char;
    type AttributeValueReferenceHex = char;
    type CharData = K::CharData;
    type CData = K::CData;
    type ReferenceNamed = K::ReferenceNamed;
    type ReferenceDecimal = char;
    type ReferenceHex = char;
    type ProcessingInstructionStart = K::ProcessingInstructionStart;
    type ProcessingInstructionValue = K::ProcessingInstructionValue;
    type Comment = K::Comment;
}

pub type ValidToken<'a, K = pull_parser::FusedKind<'a>> = Token<ValidKind<'a, K>>;

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("An XML document cannot start with this item"))]
    InvalidStartItem,

    DeclarationOnlyAllowedAtStart,

    InvalidDeclarationVersion {
        version: String,
    },

    InvalidDeclarationEncoding {
        encoding: String,
    },

    InvalidDeclarationStandalone {
        standalone: String,
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

    AttributeValueReferenceNamedUnknown {
        text: String,
    },
    ReferenceNamedUnknown {
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
    ElementNameSuffixEmpty,
    ElementNameSuffixWithoutPrefix,

    ElementCloseSuffixWithoutPrefix,

    AttributeNameEmpty,
    AttributeNameSuffixEmpty,
    AttributeNameSuffixWithoutPrefix,

    ProcessingInstructionNameEmpty,

    ElementOpenedWithoutClose {
        name: QName<String>,
    },
    ElementSelfClosedWithoutOpen,
    ElementClosedWithoutOpen,
    ElementOpenAndCloseMismatched {
        open: QName<String>,
        close: QName<String>,
    },

    NoTopLevelElements,

    MultipleTopLevelElements {
        name: QName<String>,
    },

    AttributeDuplicate {
        name: QName<String>,
    },

    NamespaceEmpty,

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
    use token::{Streaming::*, Token::*};

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
    fn fail_only_declaration_version_1_0() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.1")]);

        assert_error!(&e, Error::InvalidDeclarationVersion { version } if version == "1.1");
    }

    #[test]
    fn fail_unknown_declaration_encoding() {
        let e = ValidatorCore::validate_all(vec![
            DeclarationStart("1.0"),
            DeclarationEncoding("UTF-8<"),
        ]);

        assert_error!(&e, Error::InvalidDeclarationEncoding { encoding } if encoding == "UTF-8<");
    }

    #[test]
    fn fail_unknown_declaration_standalone() {
        let e = ValidatorCore::validate_all(vec![
            DeclarationStart("1.0"),
            DeclarationStandalone("maybe"),
        ]);

        assert_error!(&e, Error::InvalidDeclarationStandalone { standalone } if standalone == "maybe");
    }

    #[test]
    fn fail_char_data_outside_element_if_not_whitespace() {
        let e = ValidatorCore::validate_all(vec![CharData(Complete("a"))]);

        assert_error!(&e, Error::CharDataOutsideOfElement { text } if text == "a");
    }

    #[test]
    fn fail_cdata_outside_element() {
        let e = ValidatorCore::validate_all(vec![DeclarationStart("1.0"), CData(Complete("a"))]);

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
    fn fail_element_name_suffix_empty() {
        let e =
            ValidatorCore::validate_all(vec![ElementOpenStart("a"), ElementOpenStartSuffix("")]);

        assert_error!(&e, Error::ElementNameSuffixEmpty);
    }

    #[test]
    fn fail_element_name_suffix_without_prefix() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementOpenEnd,
            ElementOpenStartSuffix("b"),
        ]);

        assert_error!(&e, Error::ElementNameSuffixWithoutPrefix);
    }

    #[test]
    fn fail_close_element_name_suffix_without_prefix() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementOpenEnd,
            ElementCloseSuffix("b"),
        ]);

        assert_error!(&e, Error::ElementCloseSuffixWithoutPrefix);
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
    fn fail_attribute_name_suffix_empty() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("b"),
            AttributeStartSuffix(""),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeNameSuffixEmpty);
    }

    #[test]
    fn fail_attribute_suffix_without_prefix() {
        let e = ValidatorCore::validate_all(vec![ElementOpenStart("a"), AttributeStartSuffix("b")]);

        assert_error!(&e, Error::AttributeNameSuffixWithoutPrefix);
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
    fn fail_mismatched_open_and_close_same_prefix() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            ElementOpenStartSuffix("b"),
            ElementClose("a"),
            ElementCloseSuffix("c"),
        ]);

        assert_error!(&e, Error::ElementOpenAndCloseMismatched { open, close } if open == ("a", "b") && close == ("a", "c"));
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
            AttributeValueLiteral(Complete("c")),
            AttributeStart("b"),
            AttributeValueLiteral(Complete("d")),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeDuplicate { name } if name == "b");
    }

    #[test]
    fn may_differ_by_attribute_qname() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("b"),
            AttributeStartSuffix("x"),
            AttributeValueLiteral(Complete("c")),
            AttributeStart("b"),
            AttributeStartSuffix("y"),
            AttributeValueLiteral(Complete("d")),
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn fail_namespace_empty() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("xmlns"),
            AttributeStartSuffix("b"),
            AttributeStartComplete,
            AttributeValueEnd,
        ]);

        assert_error!(&e, Error::NamespaceEmpty);
    }

    #[test]
    fn may_have_non_empty_namespace() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("xmlns"),
            AttributeStartSuffix("b"),
            AttributeStartComplete,
            AttributeValueLiteral(Complete("uri")),
            AttributeValueEnd,
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn may_have_empty_default_namespace() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("a"),
            AttributeStart("xmlns"),
            AttributeStartComplete,
            AttributeValueEnd,
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn fail_not_quite_xml_processing_instruction() {
        let e = ValidatorCore::validate_all(vec![ProcessingInstructionStart("xMl")]);

        assert_error!(&e, Error::ProcessingInstructionInvalidName { name } if name == "xMl");
    }

    #[test]
    fn may_start_with_whitespace() {
        let e = ValidatorCore::validate_all(vec![
            CharData(Complete(" \r\n\t")),
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
    fn may_start_with_comment() {
        let e = ValidatorCore::validate_all(vec![
            Comment(""),
            ElementOpenStart("element"),
            ElementSelfClose,
        ]);

        assert_ok!(e);
    }

    #[test]
    fn fail_does_not_start_with_declaration_element_processing_instruction_or_comment() {
        let e = ValidatorCore::validate_all(vec![ReferenceNamed("lt")]);

        assert_error!(&e, Error::InvalidStartItem);
    }

    #[test]
    fn may_use_predefined_entities_in_named_reference() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("element"),
            ElementOpenEnd,
            ReferenceNamed("lt"),
            ReferenceNamed("gt"),
            ReferenceNamed("amp"),
            ReferenceNamed("quot"),
            ReferenceNamed("apos"),
            ElementClose("element"),
        ]);

        assert_ok!(e);
    }

    #[test]
    fn fail_undefined_entity_in_attribute_value_named_reference() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("element"),
            AttributeStart("attribute"),
            AttributeValueReferenceNamed("a"),
            ElementSelfClose,
        ]);

        assert_error!(&e, Error::AttributeValueReferenceNamedUnknown { text } if text == "a");
    }

    #[test]
    fn fail_undefined_entity_in_named_reference() {
        let e = ValidatorCore::validate_all(vec![
            ElementOpenStart("element"),
            ElementOpenEnd,
            ReferenceNamed("a"),
            ElementClose("element"),
        ]);

        assert_error!(&e, Error::ReferenceNamedUnknown { text } if text == "a");
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

    struct ShimTokenKind<'a>(PhantomData<&'a str>);

    impl<'a> TokenKind for ShimTokenKind<'a> {
        type DeclarationStart = &'a str;
        type DeclarationEncoding = &'a str;
        type DeclarationStandalone = &'a str;
        type ElementOpenStart = &'a str;
        type ElementOpenStartSuffix = &'a str;
        type ElementClose = &'a str;
        type ElementCloseSuffix = &'a str;
        type AttributeStart = &'a str;
        type AttributeStartSuffix = &'a str;
        type AttributeValueLiteral = Streaming<&'a str>;
        type AttributeValueReferenceNamed = &'a str;
        type AttributeValueReferenceDecimal = &'a str;
        type AttributeValueReferenceHex = &'a str;
        type CharData = Streaming<&'a str>;
        type CData = Streaming<&'a str>;
        type ReferenceNamed = &'a str;
        type ReferenceDecimal = &'a str;
        type ReferenceHex = &'a str;
        type ProcessingInstructionStart = &'a str;
        type ProcessingInstructionValue = &'a str;
        type Comment = &'a str;
    }

    type ShimToken<'a> = Token<ShimTokenKind<'a>>;

    struct Shim;

    impl<'b> Exchange<&'b str> for Shim {
        type Output<'a> = &'b str;

        fn exchange(&self, idx: &'b str) -> Self::Output<'_> {
            idx
        }
    }

    impl<'b> Exchange<Streaming<&'b str>> for Shim {
        type Output<'a> = Streaming<&'b str>;

        fn exchange(&self, idx: Streaming<&'b str>) -> Self::Output<'_> {
            idx
        }
    }

    impl ValidatorCore {
        fn validate_all<'a>(tokens: impl IntoIterator<Item = ShimToken<'a>>) -> super::Result<()> {
            let mut me = Self::default();
            for token in tokens {
                me.push(token, &Shim)?;
            }
            me.finish()
        }
    }
}
