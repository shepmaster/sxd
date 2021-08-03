#![allow(dead_code, unused_imports)]
#![deny(rust_2018_idioms)]

use pull_parser::{FusedIndex, FusedIndexToken, FusedToken, Parser};
use std::{fmt, io, marker::PhantomData, mem, str::FromStr};
use token::{Streaming, Token, TokenVariant};
use util::{QName, QNameBuilder};
use validation::Validator;

pub use derive::Mapper;

macro_rules! expect_qualified_token {
    ($($name:ident),*) => {
        paste::paste! {
            $(
                // TODO: this should return an interned string
                fn [<try_ $name:snake>](&mut self) -> Result<Option<QName<String>>> {
                    if !self.peek_index().ok_or("ran out")??.[<try_into_ $name>]().is_ok() {
                        return Ok(None);
                    }

                    let start = self
                        .expect_index()?
                        .[<try_into_ $name>]()
                        .map_err(|t| {
                            format!(
                                "Expected {} but it was {:?}",
                                stringify!([<$name>]),
                                t.variant(),
                            )
                        })?;

                    let qname = QNameBuilder::new(self.exchange_index(start).to_owned());

                    match self.expect_index()? {
                        Token::[<$name Suffix>](x) => {
                            let qname = qname.push(self.exchange_index(x).to_owned());
                            self.expect_token(TokenVariant::[<$name Complete>])?;
                            Ok(Some(qname))
                        },
                        Token::[<$name Complete>] => Ok(Some(qname.finish())),
                        t => {
                            Err(format!(
                                "Expected {} or {} but it was {:?}",
                                stringify!([<$name _Suffix>]),
                                stringify!([<$name _Complete>]),
                                t.variant(),
                            ).into())
                        }
                    }
                }

                // TODO: this should return an interned string
                fn [<expect_ $name:snake>](&mut self) -> Result<QName<String>> {
                    Ok(self.[<try_ $name:snake>]()?.ok_or("It wasn't there")?)
                }
            )*
        }
    };
}

pub struct Peeker<R> {
    head: Option<FusedIndexToken>,
    token_source: Validator<R>,
}

impl<R> Peeker<R>
where
    R: io::Read,
{
    pub fn new(token_source: Validator<R>) -> Self {
        Self {
            head: None,
            token_source,
        }
    }
}

impl<R> Peekable for Peeker<R>
where
    R: io::Read,
{
    fn exchange_index(&self, idx: FusedIndex) -> &str {
        self.token_source.inner().exchange_index(idx)
    }

    fn exchange(&self, t: FusedIndexToken) -> FusedToken<'_> {
        self.token_source.inner().exchange(t)
    }

    fn expect_index(&mut self) -> Result<FusedIndexToken> {
        self.next_index().ok_or("ran out of input")?
    }

    fn expect_str(&mut self) -> Result<FusedToken<'_>> {
        self.next_str().ok_or("ran out of input")?
    }

    fn expect_token(&mut self, variant: TokenVariant) -> Result<()> {
        let tok = self.expect_index()?;
        if tok.is_variant(variant) {
            Ok(())
        } else {
            Err(format!("Expected a {:?} but found a {:?}", variant, tok.variant()).into())
        }
    }

    fn peek_index(&mut self) -> Option<Result<FusedIndexToken>> {
        // TODO: Polonius

        if self.head.is_none() {
            let t = match self.token_source.next_index() {
                Some(Ok(t)) => t,
                Some(Err(e)) => return Some(Err(e.into())),
                None => return None,
            };

            self.head = Some(t);
        }

        Some(Ok(self.head.unwrap()))
    }

    fn peek_str(&mut self) -> Option<Result<FusedToken<'_>>> {
        self.peek_index().map(|r| r.map(move |t| self.exchange(t)))
    }

    fn next_index(&mut self) -> Option<Result<FusedIndexToken>> {
        let head = self.head.take();
        if head.is_some() {
            return head.map(Ok);
        }

        self.token_source
            .next_index()
            .map(|r| r.map_err(Into::into))
    }

    fn next_str(&mut self) -> Option<Result<FusedToken<'_>>> {
        self.next_index().map(|r| r.map(move |t| self.exchange(t)))
    }

    fn burn(&mut self) {
        self.next_index();
    }

    fn next_content(&mut self) -> Result<Option<Content<usize>>> {
        let head = self.expect_index()?;

        match head {
            Token::CharData(chardata) => Ok(Some(Content::CharData(chardata))),

            Token::ElementOpenStart(..) => {
                // TODO: Have a way of jumping into parsing without putting this back
                self.head = Some(head);
                let name = self.expect_element_open_start()?;
                Ok(Some(Content::ElementOpenStart(name)))
            }

            _ => {
                self.head = Some(head);
                Ok(None)
            }
        }
    }

    fn exchange_junk(&self, i: usize) -> &str {
        // TODO: No way it is right to use `FusedIndex::Direct` here!
        self.exchange_index(FusedIndex::Direct(i))
    }
}

pub enum Content<T> {
    ElementOpenStart(QName<String>),
    CharData(Streaming<T>),
}

pub trait Peekable {
    fn exchange_index(&self, idx: FusedIndex) -> &str;

    fn exchange(&self, t: FusedIndexToken) -> FusedToken<'_>;

    expect_qualified_token!(ElementOpenStart, ElementClose, AttributeStart);

    fn expect_index(&mut self) -> Result<FusedIndexToken>;

    fn expect_str(&mut self) -> Result<FusedToken<'_>>;

    fn expect_token(&mut self, variant: TokenVariant) -> Result<()>;

    fn peek_index(&mut self) -> Option<Result<FusedIndexToken>>;

    fn peek_str(&mut self) -> Option<Result<FusedToken<'_>>>;

    fn next_index(&mut self) -> Option<Result<FusedIndexToken>>;

    fn next_str(&mut self) -> Option<Result<FusedToken<'_>>>;

    fn burn(&mut self);

    fn next_content(&mut self) -> Result<Option<Content<usize>>>;

    fn exchange_junk(&self, junk: usize) -> &str;
}

// TODO: generic over validator; parser?
// TODO: remove double generic
pub fn decode<T>(token_source: &mut impl Peekable) -> Result<T>
where
    T: FromContent,
{
    let name = token_source.expect_element_open_start()?;
    let mut builder = T::Builder::default();

    let accumulated = builder.element(name.as_deref(), token_source)?;
    assert_eq!(accumulated, Accumulated::Yes);

    builder
        .complete()?
        .ok_or("There was nothing to parse")
        .map_err(Into::into)

    // TODO: ensure end-of-input
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Accumulated {
    Yes,
    No,
}

pub trait FromContentAccumulator {
    fn char_data(&mut self, primer: Streaming<&str>) -> Result<Accumulated> {
        let _primer = primer;
        Ok(Accumulated::No)
    }

    // TODO: Can we remove dyn dispatch here?
    fn element(
        &mut self,
        name: QName<&str>,
        token_source: &mut dyn Peekable,
    ) -> Result<Accumulated> {
        let _name = name;
        let _token_source = token_source;
        Ok(Accumulated::No)
    }
}

pub trait FromContentBuilder: FromContentAccumulator + Default {
    type Output;

    fn complete(self) -> Result<Option<Self::Output>>;
}

pub trait FromContent {
    type Builder: FromContentBuilder<Output = Self>;
}

impl<T: FromContent> FromContent for Option<T> {
    type Builder = OptionContentBuilder<T>;
}

pub struct OptionContentBuilder<T: FromContent>(Option<T::Builder>);

impl<T: FromContent> Default for OptionContentBuilder<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: FromContent> FromContentAccumulator for OptionContentBuilder<T> {
    fn char_data(&mut self, char_data: Streaming<&str>) -> Result<Accumulated> {
        self.0
            .get_or_insert_with(Default::default)
            .char_data(char_data)
    }

    fn element(
        &mut self,
        name: QName<&str>,
        token_source: &mut dyn Peekable,
    ) -> Result<Accumulated> {
        self.0
            .get_or_insert_with(Default::default)
            .element(name, token_source)
    }
}

impl<T: FromContent> FromContentBuilder for OptionContentBuilder<T> {
    type Output = Option<T>;

    fn complete(self) -> Result<Option<Self::Output>> {
        match self.0 {
            Some(b) => Ok(Some(b.complete()?)),
            None => Ok(None),
        }
    }
}

impl<T: FromContent> FromContent for Vec<T> {
    type Builder = FromContentVecBuilder<T>;
}

pub struct FromContentVecBuilder<T: FromContent> {
    builder: T::Builder,
    items: Option<Vec<T>>,
}

impl<T: FromContent> Default for FromContentVecBuilder<T> {
    fn default() -> Self {
        Self {
            builder: Default::default(),
            items: Default::default(),
        }
    }
}

// TODO: what about multiple comments? CharData? CData?
impl<T: FromContent> FromContentAccumulator for FromContentVecBuilder<T> {
    fn element(
        &mut self,
        name: QName<&str>,
        token_source: &mut dyn Peekable,
    ) -> Result<Accumulated> {
        if Accumulated::Yes == self.builder.element(name, token_source)? {
            let this_builder = mem::take(&mut self.builder);
            let this_item = this_builder.complete()?;
            // can `this_item` ever be `None`? Should we know that based on Yes return value?
            self.items
                .get_or_insert_with(Default::default)
                .extend(this_item);

            return Ok(Accumulated::Yes);
        }

        Ok(Accumulated::No)
    }
}

impl<T: FromContent> FromContentBuilder for FromContentVecBuilder<T> {
    type Output = Vec<T>;

    fn complete(self) -> Result<Option<Self::Output>> {
        Ok(self.items)
    }
}

trait FromAttributeAccumulator {
    fn literal(&mut self, s: &str);
    fn reference_named(&mut self, s: &str);
    fn reference_decimal(&mut self, s: &str);
    fn reference_hex(&mut self, s: &str);
}

trait FromAttributeBuilder: Default + FromAttributeAccumulator {
    type Output;

    fn complete(self) -> Self::Output;
}

trait FromAttribute {
    type Builder: FromAttributeBuilder<Output = Self>;
}

impl<T: FromAttribute> FromAttribute for Option<T> {
    type Builder = OptionBuilder<T>;
}

struct OptionBuilder<T: FromAttribute>(T::Builder);

impl<T: FromAttribute> Default for OptionBuilder<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: FromAttribute> FromAttributeAccumulator for OptionBuilder<T> {
    fn literal(&mut self, s: &str) {
        self.0.literal(s)
    }

    fn reference_named(&mut self, s: &str) {
        self.0.reference_named(s)
    }

    fn reference_decimal(&mut self, s: &str) {
        self.0.reference_decimal(s)
    }

    fn reference_hex(&mut self, s: &str) {
        self.0.reference_hex(s)
    }
}

impl<T: FromAttribute> FromAttributeBuilder for OptionBuilder<T> {
    type Output = Option<T>;

    fn complete(self) -> Self::Output {
        Some(self.0.complete())
    }
}

impl FromAttribute for String {
    type Builder = String;
}

impl FromAttributeAccumulator for String {
    fn literal(&mut self, s: &str) {
        self.push_str(s);
    }

    fn reference_named(&mut self, _s: &str) {
        todo!()
    }

    fn reference_decimal(&mut self, _s: &str) {
        todo!()
    }

    fn reference_hex(&mut self, _s: &str) {
        todo!()
    }
}

impl FromAttributeBuilder for String {
    type Output = String;

    fn complete(self) -> Self::Output {
        self
    }
}

impl FromContent for String {
    type Builder = StringContentBuilder;
}

#[derive(Default)]
pub struct StringContentBuilder(Option<String>);

impl FromContentAccumulator for StringContentBuilder {
    fn char_data(&mut self, char_data: Streaming<&str>) -> Result<Accumulated> {
        self.0
            .get_or_insert_with(Default::default)
            .push_str(char_data.unify());
        Ok(Accumulated::Yes)
    }
}

impl FromContentBuilder for StringContentBuilder {
    type Output = String;

    fn complete(self) -> Result<Option<Self::Output>> {
        Ok(self.0)
    }
}

macro_rules! from_content_via_parse {
    ($($ty:ty),* $(,)?) => {
        $(
            impl FromContent for $ty {
                type Builder = FromContentParseBuilder<$ty>;
            }
        )*
    };
}

from_content_via_parse!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, bool);

pub struct FromContentParseBuilder<T> {
    builder: <String as FromContent>::Builder,
    ty: PhantomData<T>,
}

impl<T> Default for FromContentParseBuilder<T> {
    fn default() -> Self {
        Self {
            builder: Default::default(),
            ty: Default::default(),
        }
    }
}

impl<T> FromContentAccumulator for FromContentParseBuilder<T> {
    fn char_data(&mut self, char_data: Streaming<&str>) -> Result<Accumulated> {
        self.builder.char_data(char_data)
    }

    fn element(
        &mut self,
        name: QName<&str>,
        token_source: &mut dyn Peekable,
    ) -> Result<Accumulated> {
        self.builder.element(name, token_source)
    }
}

impl<T> FromContentBuilder for FromContentParseBuilder<T>
where
    T: FromStr,
    T::Err: fmt::Debug,
{
    type Output = T;

    fn complete(self) -> Result<Option<Self::Output>> {
        Ok(self
            .builder
            .complete()?
            .map(|v| v.parse().expect("This is a bad value")))
    }
}

macro_rules! from_attribute_via_parse {
    ($($ty:ty),* $(,)?) => {
        $(
            impl FromAttribute for $ty {
                type Builder = ParseBuilder<$ty>;
            }
        )*
    };
}

from_attribute_via_parse!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, bool);

struct ParseBuilder<T> {
    builder: <String as FromAttribute>::Builder,
    ty: PhantomData<T>,
}

impl<T> Default for ParseBuilder<T> {
    fn default() -> Self {
        Self {
            builder: Default::default(),
            ty: Default::default(),
        }
    }
}

impl<T> FromAttributeAccumulator for ParseBuilder<T> {
    fn literal(&mut self, s: &str) {
        self.builder.literal(s)
    }

    fn reference_named(&mut self, s: &str) {
        self.builder.reference_named(s)
    }

    fn reference_decimal(&mut self, s: &str) {
        self.builder.reference_decimal(s)
    }

    fn reference_hex(&mut self, s: &str) {
        self.builder.reference_hex(s)
    }
}

impl<T> FromAttributeBuilder for ParseBuilder<T>
where
    T: FromStr,
    T::Err: fmt::Debug,
{
    type Output = T;

    fn complete(self) -> Self::Output {
        FromAttributeBuilder::complete(self.builder)
            .parse()
            .expect("This a bad value")
    }
}

type Error = Box<dyn std::error::Error>;
type Result<T, E = Error> = std::result::Result<T, E>;

fn err_bad_start(actual: &str) -> Error {
    format!("Unexpected element '{}'", actual).into()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ez_decode<T>(input: &str) -> T
    where
        T: FromContent,
    {
        let parser = Parser::new(input.as_bytes());
        let validator = Validator::new(parser);
        let mut peekable = Peeker::new(validator);

        decode(&mut peekable).expect("Decoding failed")
    }

    #[test]
    fn unit_struct_self_closed() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Alpha;

        let input = r#"<alpha />"#;
        let decoded: Alpha = ez_decode(input);

        assert_eq!(decoded, Alpha);
    }

    #[test]
    fn unit_struct_close_tag() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Beta;

        let input = r#"<beta></beta>"#;
        let decoded: Beta = ez_decode(input);

        assert_eq!(decoded, Beta);
    }

    #[test]
    fn tuple_struct_field_as_attribute() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Alpha(#[map(attribute, rename = "name")] String);

        let input = r#"<alpha name="beta" />"#;
        let decoded: Alpha = ez_decode(input);

        assert_eq!(decoded, Alpha("beta".into()));
    }

    #[test]
    fn named_struct_field_as_attribute() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Beta {
            #[map(attribute)]
            email: String,
        }

        let input = r#"<beta email="a@b.com" />"#;
        let decoded: Beta = ez_decode(input);

        assert_eq!(
            decoded,
            Beta {
                email: "a@b.com".into(),
            }
        );
    }

    #[test]
    fn tuple_struct_attributes_unordered() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Alpha(
            #[map(attribute, rename = "head")] String,
            #[map(attribute, rename = "tail")] String,
        );

        let input = r#"<alpha tail="z" head="a" />"#;
        let decoded: Alpha = ez_decode(input);

        assert_eq!(decoded, Alpha("a".into(), "z".into()));
    }

    #[test]
    fn named_struct_attributes_unordered() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Beta {
            #[map(attribute)]
            head: String,
            #[map(attribute)]
            tail: String,
        }

        let input = r#"<beta tail="z" head="a" />"#;
        let decoded: Beta = ez_decode(input);

        assert_eq!(
            decoded,
            Beta {
                head: "a".into(),
                tail: "z".into(),
            }
        );
    }

    #[test]
    fn tuple_struct_field_as_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Alpha(#[map(content)] String);

        let input = r#"<alpha>beta</alpha>"#;
        let decoded: Alpha = ez_decode(input);

        assert_eq!(decoded, Alpha("beta".into()));
    }

    #[test]
    fn named_struct_field_as_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Beta {
            #[map(content)]
            email: String,
        }

        let input = r#"<beta>a@b.com</beta>"#;
        let decoded: Beta = ez_decode(input);

        assert_eq!(
            decoded,
            Beta {
                email: "a@b.com".into(),
            }
        );
    }

    #[test]
    fn tuple_struct_struct_as_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Outer(#[map(content)] Inner);

        #[derive(Debug, PartialEq, Mapper)]
        struct Inner;

        let input = r#"<outer><inner /></outer>"#;
        let decoded: Outer = ez_decode(input);

        assert_eq!(decoded, Outer(Inner));
    }

    #[test]
    fn named_struct_struct_as_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Outer {
            #[map(content)]
            child: Inner,
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Inner;

        let input = r#"<outer><inner /></outer>"#;
        let decoded: Outer = ez_decode(input);

        assert_eq!(decoded, Outer { child: Inner });
    }

    #[test]
    fn default_attribute() {
        #[derive(Debug, PartialEq, Mapper)]
        struct IsItThere {
            #[map(attribute, default)]
            cost: u8,
            #[map(attribute, default = "|| 42")]
            answer: u8,
        }

        let input = r#"<is-it-there />"#;
        let decoded: IsItThere = ez_decode(input);

        assert_eq!(
            decoded,
            IsItThere {
                cost: 0,
                answer: 42,
            }
        );
    }

    #[test]
    fn default_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct IsItThere {
            #[map(content, default)]
            cost: String,
            #[map(content, default = r#"|| "a".to_string()"#)]
            answer: String,
        }

        let input = r#"<is-it-there />"#;
        let decoded: IsItThere = ez_decode(input);

        assert_eq!(
            decoded,
            IsItThere {
                cost: "".into(),
                answer: "a".into(),
            }
        );
    }

    #[test]
    fn default_and_required_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct IsItThere {
            #[map(content, default)]
            prefix: u8,
            #[map(content)]
            answer: Answer,
            #[map(content, default)]
            postfix: u8,
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Answer;

        let input = r#"<is-it-there><answer /></is-it-there>"#;
        let decoded: IsItThere = ez_decode(input);

        assert_eq!(
            decoded,
            IsItThere {
                prefix: 0,
                answer: Answer,
                postfix: 0,
            }
        );
    }

    #[test]
    fn whitespace_only_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Outer(#[map(content)] Inner);

        #[derive(Debug, PartialEq, Mapper)]
        struct Inner;

        let input = r#"<outer> <inner /> </outer>"#;
        let decoded: Outer = ez_decode(input);

        assert_eq!(decoded, Outer(Inner));
    }

    #[test]
    fn enum_content_unordered() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Sentence(#[map(content)] Vec<Letter>);

        #[derive(Debug, PartialEq, Mapper)]
        enum Letter {
            Alpha(Alpha),
            Beta(Beta),
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Alpha;

        #[derive(Debug, PartialEq, Mapper)]
        struct Beta;

        let input = r#"<sentence><alpha /><beta /><alpha /><beta /></sentence>"#;
        let decoded: Sentence = ez_decode(input);

        assert_eq!(
            decoded,
            Sentence(vec![
                Letter::Alpha(Alpha),
                Letter::Beta(Beta),
                Letter::Alpha(Alpha),
                Letter::Beta(Beta)
            ])
        );
    }

    #[test]
    fn optional_attribute() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Person {
            #[map(attribute, default)]
            name: Option<String>,
        }

        let input = r#"<person name="alice" />"#;
        let decoded: Person = ez_decode(input);

        assert_eq!(
            decoded,
            Person {
                name: Some("alice".into())
            }
        );

        let input = r#"<person />"#;
        let decoded: Person = ez_decode(input);

        assert_eq!(decoded, Person { name: None });
    }

    #[test]
    fn optional_content() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Person {
            #[map(content, default)]
            name: Option<String>,
        }

        let input = r#"<person>alice</person>"#;
        let decoded: Person = ez_decode(input);

        assert_eq!(
            decoded,
            Person {
                name: Some("alice".into())
            }
        );

        let input = r#"<person />"#;
        let decoded: Person = ez_decode(input);

        assert_eq!(decoded, Person { name: None });
    }

    #[test]
    fn a_vec_of_children() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Container(#[map(content)] Vec<Item>);

        #[derive(Debug, PartialEq, Mapper)]
        struct Item;

        let input = r#"<container> <item /> <item /> </container>"#;
        let decoded: Container = ez_decode(input);

        assert_eq!(decoded, Container(vec![Item, Item]));
    }

    #[test]
    fn a_vec_of_children_among_siblings() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Container {
            #[map(content)]
            head: Head,
            #[map(content)]
            items: Vec<Item>,
            #[map(content)]
            tail: Tail,
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Head;

        #[derive(Debug, PartialEq, Mapper)]
        struct Item;

        #[derive(Debug, PartialEq, Mapper)]
        struct Tail;

        let input = r#"<container> <head /> <item /> <item /> <tail /> </container>"#;
        let decoded: Container = ez_decode(input);

        assert_eq!(
            decoded,
            Container {
                head: Head,
                items: vec![Item, Item],
                tail: Tail,
            }
        );
    }

    #[test]
    fn a_vec_of_children_among_optional_siblings() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Container {
            #[map(content, default)]
            head: Option<Head>,
            #[map(content)]
            items: Vec<Item>,
            #[map(content, default)]
            tail: Option<Tail>,
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Head;

        #[derive(Debug, PartialEq, Mapper)]
        struct Item;

        #[derive(Debug, PartialEq, Mapper)]
        struct Tail;

        let input = r#"<container> <head /> <item /> <item /> <tail /> </container>"#;
        let decoded: Container = ez_decode(input);

        assert_eq!(
            decoded,
            Container {
                head: Some(Head),
                items: vec![Item, Item],
                tail: Some(Tail),
            }
        );

        let input = r#"<container> <item /> <item /> </container>"#;
        let decoded: Container = ez_decode(input);

        assert_eq!(
            decoded,
            Container {
                head: None,
                items: vec![Item, Item],
                tail: None,
            }
        );
    }

    #[test]
    fn a_little_of_everything() {
        #[derive(Debug, PartialEq, Mapper)]
        struct Person {
            #[map(attribute)]
            name: String,
            #[map(attribute, default = "|| 38")]
            age: u8,
            #[map(content)]
            address: Address,
            #[map(content)]
            profession: Profession,
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Address(
            #[map(attribute, rename = "primary")] bool,
            #[map(content)] String,
        );

        #[derive(Debug, PartialEq, Mapper)]
        enum Profession {
            Student(Student),
            Teacher(Teacher),
        }

        #[derive(Debug, PartialEq, Mapper)]
        struct Student;

        #[derive(Debug, PartialEq, Mapper)]
        struct Teacher;

        let input = r#"<person name="Alice"><address primary="true">123 Main St</address><student /></person>"#;
        let decoded: Person = ez_decode(input);

        assert_eq!(
            decoded,
            Person {
                name: "Alice".into(),
                age: 38,
                address: Address(true, "123 Main St".into()),
                profession: Profession::Student(Student),
            }
        );
    }
}

// TODO
//
// qnames
// namespace tracking
// entities, esp named ones
// xml:space
// unify name "child" / "content"
// CData
// Comments
// #[map_attr] instead of #[map(attr)] ?
// enum of text or element
//
// Doc:
// Field order matters (should we only do that for tuple struct?)
//
// child should be default
