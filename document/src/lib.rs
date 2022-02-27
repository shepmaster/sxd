#![deny(rust_2018_idioms)]
#![allow(dead_code)] // TODO: clean up the API

use pull_parser::Parser;
use snafu::Snafu;
use std::{
    cell::RefCell,
    convert::{TryFrom, TryInto},
    mem, ops,
    rc::Rc,
    str::FromStr,
};
use string_slab::{UnsafeArena, UnsafeKey};
use thunderdome::Index;
use token::{Source, Streaming, Token};
use util::{QName, QNameBuilder2};
use validation::Validator;

// TODO: Decide if `Foo` and `FooRef` pull their weight or not
// TODO: Most code should live down around `CoreStorage` so that we can avoid excessive `borrow` / `borrow_mut`
// TODO: Need to validate names/strings are valid XML
// TODO: Avoid re-interning things?

// https://github.com/LPGhatguy/thunderdome/pull/35
trait ThuderdomePolyfill {
    fn generation(self) -> u32;
}

impl ThuderdomePolyfill for Index {
    fn generation(self) -> u32 {
        (self.to_bits() >> 32 & 0x0000_FFFF) as u32
    }
}

trait InternQName {
    fn intern_qname<Q, S>(&mut self, qname: Q) -> QName<UnsafeKey>
    where
        Q: Into<QName<S>>,
        S: AsRef<str>;
}

impl InternQName for UnsafeArena {
    fn intern_qname<Q, S>(&mut self, qname: Q) -> QName<UnsafeKey>
    where
        Q: Into<QName<S>>,
        S: AsRef<str>,
    {
        qname
            .into()
            .as_ref()
            .map(AsRef::as_ref)
            .map(|v| self.intern(v))
    }
}

#[derive(Debug, Default, Clone)]
struct Storage(Rc<RefCell<CoreStorage>>); // TODO: Arc && `Mutex`?

impl Storage {
    fn access<I, T, R>(&self, index: I, f: impl FnOnce(&CoreStorage, &T) -> R) -> R
    where
        CoreStorage: std::ops::Index<I, Output = T>,
    {
        let storage = self.0.borrow();
        let thing = &storage[index];
        f(&storage, thing)
    }
}

impl PartialEq for Storage {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Storage {}

#[derive(Debug, Default)]
struct CoreStorage {
    root: Option<ElementIndex>,

    strings: UnsafeArena,
    elements: thunderdome::Arena<ElementStorage>,
    texts: thunderdome::Arena<String>,
}

impl CoreStorage {
    fn create_element(&mut self, name: QName<UnsafeKey>) -> ElementIndex {
        self.create_element_with(name, |_| {})
    }

    fn create_element_with_parent(
        &mut self,
        name: QName<UnsafeKey>,
        parent: ParentIndex,
    ) -> ElementIndex {
        self.create_element_with(name, |e| e.parent = Some(parent))
    }

    fn create_element_with(
        &mut self,
        name: QName<UnsafeKey>,
        f: impl FnOnce(&mut ElementStorage),
    ) -> ElementIndex {
        let mut e = ElementStorage {
            name,
            parent: None,
            attributes: Vec::new(),
            children: Vec::new(),
        };
        f(&mut e);
        ElementIndex(self.elements.insert(e))
    }
}

macro_rules! delegate_index {
    ($($field:ident[$key_type:ty] -> $output_type:ty,)*) => {
        $(
            impl ops::Index<$key_type> for CoreStorage {
                type Output = $output_type;

                fn index(&self, index: $key_type) -> &Self::Output {
                    self.$field.index(index.0)
                }
            }

            impl ops::IndexMut<$key_type> for CoreStorage {
                fn index_mut(&mut self, index: $key_type) -> &mut Self::Output {
                    self.$field.index_mut(index.0)
                }
            }
        )*
    };
}

delegate_index! {
    elements[ElementIndex] -> ElementStorage,
    texts[TextIndex] -> String,
}

#[derive(Debug)]
struct ElementStorage {
    name: QName<UnsafeKey>,
    parent: Option<ParentIndex>,
    // Assuming values are mostly short; not extreme number, we care about order
    // TODO: Should we create an `Attribute` type?
    attributes: Vec<(QName<UnsafeKey>, UnsafeKey)>,
    children: Vec<ChildIndex>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
struct DocumentIndex;

#[derive(Copy, Clone, PartialEq, Eq)]
struct ElementIndex(Index);

impl std::fmt::Debug for ElementIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("ElementIndex")
            .field(&self.0.generation())
            .field(&self.0.slot())
            .finish()
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct TextIndex(Index);

impl std::fmt::Debug for TextIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TextIndex")
            .field(&self.0.generation())
            .field(&self.0.slot())
            .finish()
    }
}

macro_rules! index_enum {
    (enum $name:ident {
        $($vname:ident($vtype:ty),)*
    }) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum $name {
            $($vname($vtype),)*
        }

        $(
            impl From<$vtype> for $name {
                fn from(other: $vtype) -> Self {
                    $name::$vname(other)
                }
            }

            impl TryFrom<$name> for $vtype {
                type Error = InvalidType<$name>;

                fn try_from(other: $name) -> Result<Self, Self::Error> {
                    match other {
                        $name::$vname(i) => Ok(i),
                        #[allow(unreachable_patterns)]
                        _ => Err(InvalidType { original: other }),
                    }
                }
            }

            impl PartialEq<$vtype> for $name {
                fn eq(&self, other: &$vtype) -> bool {
                    matches!(self, $name::$vname(v) if v == other)
                }
            }

            impl PartialEq<$name> for $vtype {
                fn eq(&self, other: &$name) -> bool {
                    other == self
                }
            }
        )*
    };
}

index_enum! {
    enum ParentIndex {
        Document(DocumentIndex),
        Element(ElementIndex),
    }
}

impl ParentIndex {
    const DOCUMENT: Self = ParentIndex::Document(DocumentIndex);
}

index_enum! {
    enum ChildIndex {
        Element(ElementIndex),
        Text(TextIndex),
    }
}

// TODO: move methods to here from `Document`
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct DocumentRef<'a> {
    storage: &'a Storage,
    index: DocumentIndex,
}

impl<'a> From<(&'a Storage, DocumentIndex)> for DocumentRef<'a> {
    fn from(other: (&'a Storage, DocumentIndex)) -> Self {
        let (storage, index) = other;
        DocumentRef { storage, index }
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Document {
    storage: Storage,
    index: DocumentIndex,
}

// TODO: Make this optional for those who don't want to parse text?
impl FromStr for Document {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        let parser = Parser::new(s.as_bytes());
        let mut validator = Validator::new(parser);
        Document::from_validator(&mut validator)
    }
}

impl Document {
    fn new() -> Self {
        Self::default()
    }

    pub fn from_validator<T>(validator: &mut Validator<T>) -> Result<Self>
    where
        T: std::io::Read,
    {
        // TODO: any interns in here should be checked at a higher level and shared with the validator.
        let mut storage = CoreStorage::default();

        let mut elements = Vec::new();
        let mut current_attribute_name = None;
        let mut current_attribute_value = None;

        let mut chardata = String::new();
        let mut qname = QNameBuilder2::default();

        while let Some(token) = validator.next_str() {
            use Token::*;

            match token? {
                DeclarationStart(_) => { /* no-op */ }
                DeclarationEncoding(_) => todo!(),
                DeclarationStandalone(_) => todo!(),
                DeclarationClose => { /* no-op */ }

                ElementOpenStart(name) => {
                    qname.push(storage.strings.intern(name));
                }
                ElementOpenStartSuffix(name) => {
                    qname.push(storage.strings.intern(name));
                }
                ElementOpenStartComplete => {
                    let name = qname.finish().expect("No QName");
                    let parent = elements
                        .last()
                        .copied()
                        .map(Into::into)
                        .unwrap_or(ParentIndex::DOCUMENT);
                    let element = storage.create_element_with_parent(name, parent);
                    elements.push(element)
                }
                ElementOpenEnd => { /* no-op */ }
                ElementClose(_) => { /* no-op */ }
                ElementCloseSuffix(_) => { /* no-op */ }
                ElementSelfClose | ElementCloseComplete => {
                    if let Some(e) = elements.pop() {
                        if elements.is_empty() {
                            storage.root = Some(e);
                        }
                    }
                }

                AttributeStart(name) => {
                    qname.push(storage.strings.intern(name));
                }
                AttributeStartSuffix(name) => {
                    qname.push(storage.strings.intern(name));
                }
                AttributeStartComplete => {
                    let name = qname.finish().expect("No QName");
                    current_attribute_name = Some(name);
                }
                AttributeValueLiteral(value) => {
                    let value = value.into_complete().expect("TODO");
                    let value = storage.strings.intern(value);
                    current_attribute_value = Some(value);
                }
                AttributeValueReferenceNamed(_) => todo!(),
                AttributeValueReferenceDecimal(_) => todo!(),
                AttributeValueReferenceHex(_) => todo!(),
                AttributeValueEnd => {
                    if let (Some(index), Some(name), Some(value)) = (
                        elements.last().copied(),
                        current_attribute_name.take(),
                        current_attribute_value.take(),
                    ) {
                        storage[index].attributes.push((name, value))
                    }
                }

                CharData(text) => match text {
                    Streaming::Partial(c) => chardata.push_str(c),
                    Streaming::Complete(c) => {
                        chardata.push_str(c);
                        let chardata = mem::take(&mut chardata);

                        if let Some(index) = elements.last().copied() {
                            let text = storage.texts.insert(chardata);
                            let text = TextIndex(text);
                            storage[index].children.push(text.into())
                        }
                    }
                },
                CData(_) => todo!(),

                ReferenceNamed(_) => todo!(),
                ReferenceDecimal(_) => todo!(),
                ReferenceHex(_) => todo!(),

                ProcessingInstructionStart(_) => todo!(),
                ProcessingInstructionValue(_) => todo!(),
                ProcessingInstructionEnd => todo!(),

                Comment(_) => todo!(),
            }
        }

        let storage = Storage(Rc::new(RefCell::new(storage)));
        Ok(Document {
            storage,
            index: DocumentIndex,
        })
    }

    fn as_ref(&self) -> DocumentRef<'_> {
        let Self { ref storage, index } = *self;
        (storage, index).into()
    }

    fn create_element<Q, S>(&self, name: Q) -> ElementRef<'_>
    where
        Q: Into<QName<S>>,
        S: AsRef<str>,
    {
        let mut storage = self.storage.0.borrow_mut();
        let name = storage.strings.intern_qname(name);
        let index = storage.create_element(name);
        (&self.storage, index).into()
    }

    fn root(&self) -> Option<ElementRef<'_>> {
        let index = self.storage.0.borrow().root?;
        Some((&self.storage, index).into())
    }

    // TODO: What happens when we cross between documents?
    fn set_root(&self, element: ElementRef<'_>) {
        if self.storage == *element.storage {
            element.change_parent_to(ParentIndex::DOCUMENT);

            let mut storage = self.storage.0.borrow_mut();
            storage.root = Some(element.index)
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct ElementRef<'a> {
    storage: &'a Storage,
    index: ElementIndex,
}

impl<'a> From<(&'a Storage, ElementIndex)> for ElementRef<'a> {
    fn from(other: (&'a Storage, ElementIndex)) -> Self {
        let (storage, index) = other;
        ElementRef { storage, index }
    }
}

impl<'a> ElementRef<'a> {
    fn to_owned(self) -> Element {
        let Self { storage, index } = self;
        let storage = storage.clone();
        Element { storage, index }
    }

    fn access_mut<R>(&self, f: impl FnOnce(&mut UnsafeArena, &mut ElementStorage) -> R) -> R {
        let Self {
            storage,
            index: ElementIndex(index),
        } = *self;
        let mut storage = storage.0.borrow_mut();
        let CoreStorage {
            strings, elements, ..
        } = &mut *storage;
        let element = &mut elements[index];
        f(strings, element)
    }

    fn parent(&self) -> Option<ParentRef<'a>> {
        self.storage.access(self.index, |_, element| {
            let parent = element.parent?;
            Some((self.storage, parent).into())
        })
    }

    fn children(&self) -> Vec<ChildRef<'a>> {
        self.storage.access(self.index, |_, element| {
            // TODO: avoid creating this vector when calling via `Element::children`.
            element
                .children
                .iter()
                .map(|&index| (self.storage, index).into())
                .collect()
        })
    }

    /// Removes this element from the old parents, but does not add it
    /// to the new parent.
    ///
    /// The caller is responsible for deciding *where* it is
    /// appropriate to place the child.
    fn change_parent_to(&self, parent: ParentIndex) {
        let old_parent = self.access_mut(|_, element| element.parent.replace(parent));

        if let Some(old_parent) = old_parent {
            let mut storage = self.storage.0.borrow_mut();

            match old_parent {
                ParentIndex::Document(DocumentIndex) => {
                    storage.root = None;
                }
                ParentIndex::Element(old_parent_index) => {
                    let old_parent_element = &mut storage[old_parent_index];
                    old_parent_element.children.retain(|&c| c != self.index)
                }
            }
        }
    }

    // TODO: What happens when we cross between documents?
    fn append_child(&self, child: ElementRef<'a>) {
        if self.storage == child.storage {
            child.change_parent_to(self.index.into());

            self.access_mut(|_, element| {
                element.children.push(child.index.into());
            });
        }
    }

    /// ```rust
    /// todo!("Document unsafety");
    ///
    fn name(&self) -> QName<&'a str> {
        self.storage.access(self.index, |storage, element| unsafe {
            element.name.map(|v| storage.strings.as_unbound_str(v))
        })
    }

    fn set_name<Q, S>(&self, name: Q)
    where
        Q: Into<QName<S>>,
        S: AsRef<str>,
    {
        self.access_mut(|strings, element| {
            let name = strings.intern_qname(name);
            element.name = name;
        })
    }

    /// ```rust
    /// todo!("Document unsafety");
    ///
    fn attribute_value<Q, S>(&self, attribute_name: Q) -> Option<&'a str>
    where
        Q: Into<QName<S>>,
        S: AsRef<str>,
    {
        let attribute_name = attribute_name.into();
        let attribute_name = attribute_name.as_ref().map(AsRef::as_ref);
        self.storage.access(self.index, |storage, element| {
            element.attributes.iter().find_map(|&(name, value)| {
                let this_name = unsafe { name.map(|v| storage.strings.as_str(v)) };

                (attribute_name == this_name)
                    .then(|| unsafe { storage.strings.as_unbound_str(value) })
            })
        })
    }

    fn text_value(&self) -> String {
        self.storage.access(self.index, |storage, element| {
            if let Some(c) = element.children.last().copied() {
                assert_eq!(1, element.children.len(), "TODO: Handle different lengths");
                let TextIndex(index) = c.try_into().expect("TODO");
                // TODO: how can we avoid this clone
                storage.texts[index].clone()
            } else {
                String::new()
            }
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Element {
    storage: Storage,
    index: ElementIndex,
}

impl Element {
    fn as_ref(&self) -> ElementRef<'_> {
        let Self { ref storage, index } = *self;
        (storage, index).into()
    }

    fn name(&self) -> QName<&str> {
        self.as_ref().name()
    }

    fn attribute_value<Q, S>(&self, attribute_name: Q) -> Option<&str>
    where
        Q: Into<QName<S>>,
        S: AsRef<str>,
    {
        self.as_ref().attribute_value(attribute_name)
    }

    fn text_value(&self) -> String {
        self.as_ref().text_value()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct TextRef<'a> {
    storage: &'a Storage,
    index: TextIndex,
}

impl<'a> From<(&'a Storage, TextIndex)> for TextRef<'a> {
    fn from(other: (&'a Storage, TextIndex)) -> Self {
        let (storage, index) = other;
        TextRef { storage, index }
    }
}

macro_rules! ref_enum {
    (#[index = $indextype:ident]
     enum $name:ident<'a> {
         $($vname:ident($vtype:ident<'a>),)*
     }) => {
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum $name<'a> {
            $($vname($vtype<'a>),)*
        }

        $(
            impl<'a> From<$vtype<'a>> for $name<'a> {
                fn from(other: $vtype<'a>) -> Self {
                    $name::$vname(other)
                }
            }

            impl<'a> TryFrom<$name<'a>> for $vtype<'a> {
                type Error = InvalidType<$name<'a>>;

                fn try_from(other: $name<'a>) -> Result<Self, Self::Error> {
                    match other {
                        $name::$vname(i) => Ok(i),
                        #[allow(unreachable_patterns)]
                        _ => Err(InvalidType { original: other }),
                    }
                }
            }

            impl<'a> PartialEq<$vtype<'a>> for $name<'a> {
                fn eq(&self, other: &$vtype<'a>) -> bool {
                    matches!(self, $name::$vname(v) if v == other)
                }
            }

            impl<'a> PartialEq<$name<'a>> for $vtype<'a> {
                fn eq(&self, other: &$name<'a>) -> bool {
                    other == self
                }
            }
        )*

        impl<'a> From<(&'a Storage, $indextype)> for $name<'a> {
            fn from(other: (&'a Storage, $indextype)) -> Self {
                let (storage, index) = other;
                match index {
                    $($indextype::$vname(index) => $name::$vname((storage, index).into()),)*
                }
            }
        }
    };
}

ref_enum! {
    #[index = ParentIndex]
    enum ParentRef<'a> {
        Document(DocumentRef<'a>),
        Element(ElementRef<'a>),
    }
}

ref_enum! {
    #[index = ChildIndex]
    enum ChildRef<'a> {
        Element(ElementRef<'a>),
        Text(TextRef<'a>),
    }
}

#[derive(Debug, Snafu)]
struct InvalidType<T> {
    original: T,
}

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(context(false))]
    Validation { source: validation::Error },
}

type Result<T, E = Error> = std::result::Result<T, E>;

#[cfg(test)]
mod tests {
    use super::*;

    type Error = Box<dyn std::error::Error>;
    type Result<T = (), E = Error> = std::result::Result<T, E>;

    mod parsing {
        use super::*;

        #[test]
        fn empty_self_closed_element() -> Result {
            let doc = Document::from_str(r#"<a />"#)?;
            let root = doc.root().expect("Root missing");

            assert_eq!("a", root.name());
            assert_eq!(Some(doc.as_ref().into()), root.parent());

            Ok(())
        }

        #[test]
        fn element_with_one_attribute() -> Result {
            let doc = Document::from_str(r#"<a b="c" />"#)?;
            let root = doc.root().expect("Root missing");

            assert_eq!(Some("c"), root.attribute_value("b"));

            Ok(())
        }

        #[test]
        fn element_with_text_child() -> Result {
            let doc = Document::from_str(r#"<a>hello</a>"#)?;
            let root = doc.root().expect("Root missing");

            assert_eq!("hello", root.text_value());

            Ok(())
        }
    }

    #[test]
    fn setting_the_root_element() {
        let doc = Document::new();

        assert!(doc.root().is_none());

        let root = doc.create_element("alpha");
        assert!(root.parent().is_none());

        doc.set_root(root);
        assert_eq!(Some(doc.as_ref().into()), root.parent());

        let root = doc.root().expect("Root missing");
        assert_eq!("alpha", root.name());
    }

    #[test]
    fn reparenting_an_element() {
        let doc = Document::new();

        let parent1 = doc.create_element("parent1");
        assert!(parent1.children().is_empty());

        let parent2 = doc.create_element("parent2");
        assert!(parent2.children().is_empty());

        let child = doc.create_element("child");
        assert!(child.parent().is_none());

        parent1.append_child(child);
        assert_eq!(&[child][..], parent1.children());
        assert!(parent2.children().is_empty());
        assert_eq!(Some(parent1.into()), child.parent());

        parent2.append_child(child);
        assert_eq!(&[child][..], parent2.children());
        assert!(parent1.children().is_empty());
        assert_eq!(Some(parent2.into()), child.parent());
    }

    #[test]
    fn setting_an_elements_name() {
        let doc = Document::new();

        let element = doc.create_element("alpha");
        assert_eq!("alpha", element.name());

        element.set_name("beta");
        assert_eq!("beta", element.name());
    }
}