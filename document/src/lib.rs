#![deny(rust_2018_idioms)]
// Ouroboros uses `.await` internally
#![allow(clippy::await_holding_refcell_ref)]
#![allow(dead_code)] // TODO: clean up the API

use pull_parser::Parser;
use snafu::Snafu;
use std::{
    cell::{Ref, RefCell},
    convert::TryFrom,
    mem,
    ops::{self, Deref},
    rc::Rc,
    str::FromStr,
    vec,
};
use string_slab::{UnsafeArena, UnsafeKey};
use thunderdome::Index;
use token::{Source, Streaming, Token, UniformToken};
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

    fn ref_str(
        &self,
        str_builder: impl for<'a> FnOnce(&'a Ref<'a, CoreStorage>) -> &'a str,
    ) -> StorageStr {
        StorageStrBuilder {
            storage: self.clone(),
            the_ref_builder: |s| s.0.borrow(),
            str_builder,
        }
        .build()
    }
}

impl PartialEq for Storage {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Storage {}

#[ouroboros::self_referencing]
struct StorageStr {
    storage: Storage,
    #[borrows(storage)]
    #[covariant]
    the_ref: Ref<'this, CoreStorage>,
    #[borrows(the_ref)]
    str: &'this str,
}

impl Deref for StorageStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.borrow_str()
    }
}

#[derive(Debug, Default)]
struct CoreStorage {
    preamble: Vec<CommentIndex>,
    root: Option<ElementIndex>,
    postamble: Vec<CommentIndex>,

    strings: UnsafeArena,
    elements: thunderdome::Arena<ElementStorage>,
    attributes: thunderdome::Arena<AttributeStorage>,
    texts: thunderdome::Arena<TextStorage>,
    comments: thunderdome::Arena<CommentStorage>,
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

    fn create_attribute_with_parent(
        &mut self,
        name: QName<UnsafeKey>,
        value: UnsafeKey,
        parent: ElementIndex,
    ) -> AttributeIndex {
        let index = self.attributes.insert(AttributeStorage {
            name,
            value,
            parent: Some(parent),
        });
        AttributeIndex(index)
    }

    fn create_text_with_parent(&mut self, value: String, parent: ElementIndex) -> TextIndex {
        let index = self.texts.insert(TextStorage {
            value,
            parent: Some(parent),
        });
        TextIndex(index)
    }

    fn create_comment_with_parent(&mut self, value: String, parent: ParentIndex) -> CommentIndex {
        let index = self.comments.insert(CommentStorage {
            value,
            parent: Some(parent),
        });
        CommentIndex(index)
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
    attributes[AttributeIndex] -> AttributeStorage,
    texts[TextIndex] -> TextStorage,
    comments[CommentIndex] -> CommentStorage,
}

#[derive(Debug)]
struct ElementStorage {
    name: QName<UnsafeKey>,
    parent: Option<ParentIndex>,
    // Assuming we care about order and there's not a huge number
    attributes: Vec<AttributeIndex>,
    children: Vec<ChildIndex>,
}

#[derive(Debug)]
struct AttributeStorage {
    name: QName<UnsafeKey>,
    // Assuming values are mostly short
    value: UnsafeKey,
    parent: Option<ElementIndex>,
}

#[derive(Debug)]
struct TextStorage {
    value: String,
    parent: Option<ElementIndex>,
}

#[derive(Debug)]
struct CommentStorage {
    value: String,
    parent: Option<ParentIndex>,
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
struct AttributeIndex(Index);

impl std::fmt::Debug for AttributeIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("AttributeIndex")
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

#[derive(Copy, Clone, PartialEq, Eq)]
struct CommentIndex(Index);

impl std::fmt::Debug for CommentIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CommentIndex")
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
        Comment(CommentIndex),
    }
}

index_enum! {
    enum DocumentChildIndex {
        Element(ElementIndex),
        Comment(CommentIndex),
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
        let mut comment = String::new();
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
                    if let Some(e_idx) = elements.pop() {
                        // TODO write test for `Some` case missing
                        match elements.last() {
                            Some(&p_idx) => {
                                let p = &mut storage[p_idx];
                                p.children.push(e_idx.into());
                                let e = &mut storage[e_idx];
                                e.parent = Some(p_idx.into());
                            }
                            None => storage.root = Some(e_idx),
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
                        let attribute = storage.create_attribute_with_parent(name, value, index);
                        storage[index].attributes.push(attribute);
                    }
                }

                CharData(text) => match text {
                    Streaming::Partial(c) => chardata.push_str(c),
                    Streaming::Complete(c) => {
                        chardata.push_str(c);
                        let chardata = mem::take(&mut chardata);

                        if let Some(index) = elements.last().copied() {
                            let text = storage.create_text_with_parent(chardata, index);
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

                Comment(text) => match text {
                    Streaming::Partial(text) => comment.push_str(text),
                    Streaming::Complete(text) => {
                        comment.push_str(text);
                        let comment = mem::take(&mut comment);

                        if let Some(index) = elements.last().copied() {
                            let comment = storage.create_comment_with_parent(comment, index.into());
                            storage[index].children.push(comment.into())
                        } else {
                            let comment =
                                storage.create_comment_with_parent(comment, ParentIndex::DOCUMENT);
                            let location = if storage.root.is_none() {
                                &mut storage.preamble
                            } else {
                                &mut storage.postamble
                            };
                            location.push(comment);
                        }
                    }
                },
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

    fn children(&self) -> Vec<DocumentChildRef<'_>> {
        let storage = self.storage.0.borrow();
        let CoreStorage {
            preamble,
            root,
            postamble,
            ..
        } = &*storage;

        let preamble = preamble
            .iter()
            .map(|&index| CommentRef::from((&self.storage, index)).into());
        let root = root
            .iter()
            .map(|&index| ElementRef::from((&self.storage, index)).into());
        let postamble = postamble
            .iter()
            .map(|&index| CommentRef::from((&self.storage, index)).into());

        preamble.chain(root).chain(postamble).collect()
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

    pub fn tokens<R>(&self, f: impl FnOnce(Tokens<'_>) -> R) -> R {
        let storage = self.storage.0.borrow();
        f(Tokens::new(&storage))
    }
}

#[derive(Debug)]
pub struct Tokens<'a> {
    storage: &'a CoreStorage,
    output: vec::IntoIter<UniformToken<&'a str>>,
}

impl<'a> Tokens<'a> {
    fn new(storage: &'a CoreStorage) -> Self {
        use Token as T;

        let mut output = vec![
            T::DeclarationStart("1.0"),
            // TODO encoding; standalone
            T::DeclarationClose,
        ];

        if let Some(e) = storage.root {
            one_element(storage, e, &mut output);
        }

        let output = output.into_iter();

        Self { storage, output }
    }

    pub fn next_str(&mut self) -> Option<UniformToken<&'a str>> {
        self.output.next()
    }
}

fn one_element<'a>(
    storage: &'a CoreStorage,
    e: ElementIndex,
    output: &mut Vec<UniformToken<&'a str>>,
) {
    use Token as T;

    let e = &storage[e];
    one_qname(
        e.name,
        storage,
        T::ElementOpenStart,
        T::ElementOpenStartSuffix,
        T::ElementOpenStartComplete,
        output,
    );
    for &a_idx in &e.attributes {
        let a = &storage[a_idx];
        one_qname(
            a.name,
            storage,
            T::AttributeStart,
            T::AttributeStartSuffix,
            T::AttributeStartComplete,
            output,
        );

        // TODO: escaping of things here
        unsafe { output.push(T::AttributeValueLiteral(storage.strings.as_str(a.value))) }
        output.push(T::AttributeValueEnd);
    }
    match &*e.children {
        [] => output.push(T::ElementSelfClose),
        c => {
            output.push(T::ElementOpenEnd);

            for &c in c {
                match c {
                    ChildIndex::Element(e) => one_element(storage, e, output),
                    ChildIndex::Text(t) => one_text(storage, t, output),
                    ChildIndex::Comment(_) => todo!(),
                }
            }

            one_qname(
                e.name,
                storage,
                T::ElementClose,
                T::ElementCloseSuffix,
                T::ElementCloseComplete,
                output,
            );
        }
    }
}

fn one_text<'a>(storage: &'a CoreStorage, t: TextIndex, output: &mut Vec<UniformToken<&'a str>>) {
    use Token as T;

    let t = &storage[t];

    // TODO: escaping of things here
    output.push(T::CharData(&*t.value))
}

fn one_qname<'a>(
    name: QName<UnsafeKey>,
    storage: &'a CoreStorage,
    a: fn(&'a str) -> UniformToken<&'a str>,
    b: fn(&'a str) -> UniformToken<&'a str>,
    c: UniformToken<&'a str>,

    output: &mut Vec<UniformToken<&'a str>>,
) {
    unsafe {
        match name.prefix {
            Some(prefix) => {
                output.push(a(storage.strings.as_str(prefix)));
                output.push(b(storage.strings.as_str(name.local_part)));
            }
            None => {
                output.push(a(storage.strings.as_str(name.local_part)));
            }
        }

        output.push(c);
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

    fn attributes(&self) -> Vec<AttributeRef<'a>> {
        self.storage.access(self.index, |_, element| {
            element
                .attributes
                .iter()
                .map(|&index| (self.storage, index).into())
                .collect()
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
            element.attributes.iter().find_map(|&attr| {
                let AttributeStorage { name, value, .. } = storage[attr];

                let this_name = unsafe { name.map(|v| storage.strings.as_str(v)) };

                (attribute_name == this_name)
                    .then(|| unsafe { storage.strings.as_unbound_str(value) })
            })
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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct AttributeRef<'a> {
    storage: &'a Storage,
    index: AttributeIndex,
}

impl<'a> From<(&'a Storage, AttributeIndex)> for AttributeRef<'a> {
    fn from(other: (&'a Storage, AttributeIndex)) -> Self {
        let (storage, index) = other;
        AttributeRef { storage, index }
    }
}

impl<'a> AttributeRef<'a> {
    /// ```rust
    /// todo!("Document unsafety");
    ///
    fn name(&self) -> QName<&'a str> {
        self.storage.access(self.index, |storage, attribute| {
            attribute
                .name
                .map(|v| unsafe { storage.strings.as_unbound_str(v) })
        })
    }

    /// ```rust
    /// todo!("Document unsafety");
    ///
    fn value(&self) -> &'a str {
        self.storage
            .access(self.index, |storage, attribute| unsafe {
                storage.strings.as_unbound_str(attribute.value)
            })
    }

    fn parent(&self) -> Option<ElementRef<'a>> {
        self.storage.access(self.index, |_, attribute| {
            attribute.parent.map(|e| (self.storage, e).into())
        })
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

impl<'a> TextRef<'a> {
    fn value(&self) -> impl Deref<Target = str> {
        self.storage.ref_str(|the_ref| &the_ref[self.index].value)
    }

    fn parent(&self) -> Option<ElementRef<'a>> {
        self.storage.access(self.index, |_, text| {
            text.parent.map(|text| (self.storage, text).into())
        })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct CommentRef<'a> {
    storage: &'a Storage,
    index: CommentIndex,
}

impl<'a> From<(&'a Storage, CommentIndex)> for CommentRef<'a> {
    fn from(other: (&'a Storage, CommentIndex)) -> Self {
        let (storage, index) = other;
        CommentRef { storage, index }
    }
}

impl<'a> CommentRef<'a> {
    fn value(&self) -> impl Deref<Target = str> {
        self.storage.ref_str(|the_ref| &the_ref[self.index].value)
    }

    fn parent(&self) -> Option<ParentRef<'a>> {
        self.storage.access(self.index, |_, comment| {
            comment.parent.map(|index| (self.storage, index).into())
        })
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
        Comment(CommentRef<'a>),
    }
}

ref_enum! {
    #[index = DocumentChildIndex]
    enum DocumentChildRef<'a> {
        Element(ElementRef<'a>),
        Comment(CommentRef<'a>),
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
    use std::convert::TryInto;

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
        fn element_with_one_attribute_value() -> Result {
            let doc = Document::from_str(r#"<a b="c" />"#)?;
            let root = doc.root().expect("Root missing");

            assert_eq!(Some("c"), root.attribute_value("b"));

            Ok(())
        }

        #[test]
        fn element_with_one_attribute() -> Result {
            let doc = Document::from_str(r#"<a b="c" />"#)?;
            let root = doc.root().expect("Root missing");

            let attr = root.attributes().pop().expect("Attribute missing");
            assert_eq!("b", attr.name());
            assert_eq!("c", attr.value());
            assert_eq!(Some(root), attr.parent());

            Ok(())
        }

        #[test]
        fn element_with_text_child() -> Result {
            let doc = Document::from_str(r#"<a>hello</a>"#)?;
            let root = doc.root().expect("Root missing");

            let text: TextRef<'_> = root
                .children()
                .pop()
                .expect("No child")
                .try_into()
                .expect("Not text");
            assert_eq!("hello", &*text.value());
            assert_eq!(Some(root), text.parent());

            Ok(())
        }

        #[test]
        fn element_with_comment() -> Result {
            let doc = Document::from_str(r#"<a><!-- hello --></a>"#)?;
            let root = doc.root().expect("Root missing");

            let comment: CommentRef<'_> = root
                .children()
                .pop()
                .expect("Comment missing")
                .try_into()
                .expect("Not a comment");
            assert_eq!(" hello ", &*comment.value());
            assert_eq!(Some(root.into()), comment.parent());

            Ok(())
        }

        #[test]
        fn top_level_comments() -> Result {
            let doc = Document::from_str(r#"<!--pre--><a /><!--post-->"#)?;

            let children = doc.children();
            let pre_comment: CommentRef<'_> =
                children.first().copied().unwrap().try_into().unwrap();
            let post_comment: CommentRef<'_> =
                children.last().copied().unwrap().try_into().unwrap();

            assert_eq!("pre", &*pre_comment.value());
            assert_eq!(Some(doc.as_ref().into()), pre_comment.parent());

            assert_eq!("post", &*post_comment.value());
            assert_eq!(Some(doc.as_ref().into()), post_comment.parent());

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
