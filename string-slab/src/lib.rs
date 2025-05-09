//! Provides a string interning pool tailored for XML documents

#![deny(rust_2018_idioms)]
#![deny(missing_docs)]

use hashbrown::HashSet;
use std::{
    borrow::Borrow,
    cell::RefCell,
    cmp::Ordering,
    collections::LinkedList,
    hash::{Hash, Hasher},
    mem,
    ops::Index,
    ptr::{self, NonNull},
    slice, str,
};

/// # Safety
///
/// ⚠ **This type unsafely implements safe traits** ⚠. Extreme care should be taken when using any
/// trait, even if it is not marked as unsafe.
#[derive(Debug, Copy, Clone, Eq)]
struct RawStr(*const u8, usize);

impl RawStr {
    /// # Safety
    ///
    /// We contain a pointer that's only valid while the slabs are alive, so the caller must
    /// guarantee that.
    unsafe fn as_str(&self) -> &str {
        self.as_unbound_str()
    }

    /// # Safety
    ///
    /// We contain a pointer that's only valid while the slabs are alive, so the caller must
    /// guarantee that. This method allows the *caller* to decide what the lifetime should be,
    /// and should be used with great caution.
    unsafe fn as_unbound_str<'a>(&self) -> &'a str {
        let s = slice::from_raw_parts(self.0, self.1);
        str::from_utf8_unchecked(s)
    }
}

impl PartialEq for RawStr {
    fn eq(&self, other: &Self) -> bool {
        // We only care about pointer equality as we don't reuse a pointer for more than one string
        // value.
        ptr::eq(self.0, other.0)
    }
}

/// SAFETY: This is **not** actually safe. The user of `RawStr` must ensure that the safety contract
/// of [`Self::as_str`] is upheld to use this trait.
impl Hash for RawStr {
    fn hash<H>(&self, h: &mut H)
    where
        H: Hasher,
    {
        unsafe { self.as_str().hash(h) }
    }
}

/// SAFETY: This is **not** actually safe. The user of `RawStr` must ensure that the safety contract
/// of [`Self::as_str`] is upheld to use this trait.
impl Borrow<str> for RawStr {
    fn borrow(&self) -> &str {
        unsafe { self.as_str() }
    }
}

#[derive(Debug)]
struct StringSlabBuilder {
    slab: StringSlab,
    slab_size: usize,
    len: usize,
}

enum AppendStatus {
    Appended(RawStr),
    Cycled(StringSlab, RawStr),
}

impl StringSlabBuilder {
    fn with_slab_size(slab_size: usize) -> Self {
        Self {
            slab: StringSlab::with_capacity(slab_size),
            slab_size,
            len: 0,
        }
    }

    fn cycle(&mut self) -> StringSlab {
        let new = StringSlab::with_capacity(self.slab_size);
        self.len = 0;
        mem::replace(&mut self.slab, new)
    }

    fn append(&mut self, v: &[u8]) -> AppendStatus {
        let fits_in_slab = v.len() <= self.slab_size;
        let fits_in_remaining_capacity = v.len() <= self.slab.capacity - self.len;

        match (fits_in_slab, fits_in_remaining_capacity) {
            (false, _) => {
                // Create a new slab of the exact size. Continue using our current slab so we
                // don't need to worry about updating the offset or wasting any remaining space.

                let mut slab = StringSlab::with_capacity(v.len());

                // SAFETY: The slab is guaranteed to be the same length as the incoming string.
                unsafe {
                    let s = slab.append(0, v);
                    AppendStatus::Cycled(slab, s)
                }
            }

            (_, false) => {
                // Adding the new data would overflow the slab, so we cycle into a new slab.

                let slab = self.cycle();

                // SAFETY: We've created a new slab, so we are guaranteed that the incoming
                // string can fit.
                unsafe {
                    let s = self.slab.append(self.len, v);
                    self.len += v.len();
                    AppendStatus::Cycled(slab, s)
                }
            }

            (_, true) => {
                // We have enough space in the current slab to add the new data.
                //
                // SAFETY: We've checked that there's enough space for the incoming string to
                // fit, and we've updated our length so we know we aren't aliasing existing
                // data.
                unsafe {
                    let s = self.slab.append(self.len, v);
                    self.len += v.len();
                    AppendStatus::Appended(s)
                }
            }
        }
    }
}

#[derive(Debug)]
struct StringSlab {
    data: NonNull<u8>,
    capacity: usize,
}

impl StringSlab {
    fn with_capacity(capacity: usize) -> Self {
        let mut v = Vec::<u8>::with_capacity(capacity);
        // SAFETY: Since this pointer came from a `Vec` that has an allocation, it must be
        // `NonNull`
        let data = unsafe { NonNull::new_unchecked(v.as_mut_ptr()) };
        mem::forget(v);

        Self { data, capacity }
    }

    /// # Safety
    ///
    /// The data must fit within the remaining capacity after accounting for any used data. The
    /// returned value's lifetime is unbound and must be properly constrained by the caller.
    unsafe fn append(&mut self, offset: usize, v: &[u8]) -> RawStr {
        let head = self.data.as_ptr().add(offset);
        ptr::copy_nonoverlapping(v.as_ptr(), head, v.len());
        RawStr(head, v.len())
    }
}

impl Drop for StringSlab {
    fn drop(&mut self) {
        // SAFETY: because we take a `&mut self`, we know that we can be the only
        // reference. Since we are only `u8` data, it's fine to treat the capacity as the
        // length.
        unsafe { Vec::from_raw_parts(self.data.as_ptr(), self.capacity, self.capacity) };
    }
}

/// An opaque handle to an interned string.
///
/// Use [`UnsafeArena::as_str`] if you need the string data.
#[derive(Debug, Copy, Clone, Eq)]
pub struct UnsafeKey(RawStr);

impl UnsafeKey {
    fn compare_key(&self) -> (usize, *const u8) {
        let RawStr(ptr, len) = self.0;
        (len, ptr)
    }
}

impl PartialEq for UnsafeKey {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Hash for UnsafeKey {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.compare_key().hash(state)
    }
}

impl PartialOrd for UnsafeKey {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for UnsafeKey {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare_key().cmp(&other.compare_key())
    }
}

/// A string interning pool.
#[derive(Debug)]
pub struct UnsafeArena {
    lookup: HashSet<RawStr>,
    current_slab: StringSlabBuilder,
    slabs: LinkedList<StringSlab>,
}

impl Default for UnsafeArena {
    fn default() -> Self {
        Self::new()
    }
}

impl UnsafeArena {
    /// The default size of a memory slab.
    pub const DEFAULT_SLAB_SIZE: usize = 128 * 1024;

    /// Creates an empty pool using the [default slab size](Self::DEFAULT_SLAB_SIZE).
    pub fn new() -> Self {
        Self::with_slab_size(Self::DEFAULT_SLAB_SIZE)
    }

    /// Creates an empty pool using the specified slab size.
    pub fn with_slab_size(slab_size: usize) -> Self {
        Self {
            lookup: Default::default(),
            current_slab: StringSlabBuilder::with_slab_size(slab_size),
            slabs: Default::default(),
        }
    }

    /// Add a string to the pool.
    ///
    /// If it's not already present, the string will be copied to the pool. The returned
    /// [`UnsafeKey`] can be used to tell if two strings are identical or get the string data.
    pub fn intern(&mut self, s: &str) -> UnsafeKey {
        let Self {
            lookup,
            current_slab,
            slabs,
        } = self;

        let raw_str = *lookup.get_or_insert_with(s, |s| match current_slab.append(s.as_bytes()) {
            AppendStatus::Appended(raw_str) => raw_str,
            AppendStatus::Cycled(slab, raw_str) => {
                slabs.push_front(slab);
                raw_str
            }
        });

        UnsafeKey(raw_str)
    }

    /// Convert an [`UnsafeKey`] into a string.
    ///
    /// # Safety
    ///
    /// Nothing enforces that this key came from this arena. If it did not, then this will cause
    /// undefined behavior.
    pub unsafe fn as_str(&self, key: UnsafeKey) -> &str {
        self.as_unbound_str(key)
    }

    /// Convert an [`UnsafeKey`] into a string.
    ///
    /// # Safety
    ///
    /// Nothing enforces that this key came from this arena. If it did not, then this will cause
    /// undefined behavior. This method allows the *caller* to decide what the lifetime should be,
    /// and should be used with great caution.
    unsafe fn as_unbound_str<'a>(&self, key: UnsafeKey) -> &'a str {
        key.0.as_unbound_str()
    }
}

/// Provides a safe interface to an arena by ensuring that all returned references are tied to the
/// arena.
///
/// This arena may not be suitable for cases where you need to move the arena after interning
/// values.
///
/// # Examples
///
/// ```rust
/// # use sxd_string_slab::RootedArena;
/// use std::ptr;
///
/// let input_1 = &*String::from("hello");
/// let input_2 = &*String::from("hello");
///
/// let arena = RootedArena::new();
///
/// let output_1 = arena.intern(input_1);
/// let output_2 = arena.intern(input_2);
///
/// assert_eq!("hello", output_1);
/// assert_eq!("hello", output_2);
///
/// assert!(!ptr::eq(input_1, input_2), "Input strings refer to the same value");
/// assert!(ptr::eq(output_1, output_2), "Output strings do not refer to the same value");
/// ```
#[derive(Debug, Default)]
pub struct RootedArena(RefCell<UnsafeArena>);

impl RootedArena {
    /// Creates an empty pool using the [default slab size](UnsafeArena::DEFAULT_SLAB_SIZE).
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates an empty pool using the specified slab size.
    pub fn with_slab_size(slab_size: usize) -> Self {
        Self(RefCell::new(UnsafeArena::with_slab_size(slab_size)))
    }

    /// Add a string to the pool.
    ///
    /// If it's not already present, the string will be copied to the pool.
    pub fn intern(&self, s: &str) -> &str {
        let mut arena = self.0.borrow_mut();

        // SAFETY: We can trivially show that this key came from this arena, and we tie the lifetime
        // to ourselves, which is also how long the `UnsafeArena` will last.
        unsafe {
            let key = arena.intern(s);
            arena.as_unbound_str(key)
        }
    }
}

/// Provides a safe interface to an arena by ensuring that all keys came from this arena.
///
/// # Examples
///
/// ```rust
/// # use sxd_string_slab::CheckedArena;
/// let arena = CheckedArena::new();
/// let another_arena = CheckedArena::new();
///
/// let key = arena.intern("hello");
///
/// // Use `as_str` to get a string back from the arena. If the wrong arena is used, this will
/// // return `None`.
/// assert_eq!(Some("hello"), arena.as_str(key));
/// assert_eq!(None, another_arena.as_str(key));
///
/// // Use indexing syntax if you are sure the key came from this arena.
/// assert_eq!("hello", &arena[key]);
/// ```
#[derive(Debug, Default)]
pub struct CheckedArena(Box<RefCell<UnsafeArena>>);

impl CheckedArena {
    /// Creates an empty pool using the [default slab size](UnsafeArena::DEFAULT_SLAB_SIZE).
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates an empty pool using the specified slab size.
    pub fn with_slab_size(slab_size: usize) -> Self {
        Self(Box::new(RefCell::new(UnsafeArena::with_slab_size(
            slab_size,
        ))))
    }

    /// Add a string to the pool.
    ///
    /// If it's not already present, the string will be copied to the pool. The returned
    /// [`CheckedKey`] can be used to tell if two strings are identical or get the string data.
    pub fn intern(&self, s: &str) -> CheckedKey {
        let key = self.0.borrow_mut().intern(s);
        CheckedKey(&*self.0, key)
    }

    /// Convert a [`CheckedKey`] into a string.
    ///
    /// If the `CheckedKey` did not come from this arena, `None` is returned.
    pub fn as_str(&self, key: CheckedKey) -> Option<&str> {
        // SAFETY: We ensure that the key comes from the same arena that it was generated from, and
        // we tie the lifetime to ourselves, so it cannot outlast the arena.
        unsafe {
            if ptr::eq(&*self.0, key.0) {
                Some(RefCell::borrow(&self.0).as_unbound_str(key.1))
            } else {
                None
            }
        }
    }
}

impl Index<CheckedKey> for CheckedArena {
    type Output = str;
    fn index(&self, key: CheckedKey) -> &str {
        self.as_str(key).expect("The key is not from this arena")
    }
}

/// An opaque handle to an interned string.
///
/// Use [`CheckedArena::as_str`] if you need the string data.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CheckedKey(*const RefCell<UnsafeArena>, UnsafeKey);

#[cfg(test)]
mod test {
    use hashbrown::HashMap;
    use proptest::prelude::*;
    use rand::seq::SliceRandom;
    use std::sync::LazyLock;

    use super::*;

    #[test]
    fn interning_twice_creates_equal_values() {
        let mut arena = UnsafeArena::default();
        let a = arena.intern("hello");
        let b = arena.intern("hello");
        assert_eq!(a, b);
    }

    #[test]
    fn interning_two_values_creates_non_equal_values() {
        let mut arena = UnsafeArena::default();
        let a = arena.intern("hello");
        let b = arena.intern("world");
        assert_ne!(a, b);
    }

    #[test]
    fn interning_preserves_string_values() {
        let mut arena = UnsafeArena::default();
        let a = arena.intern("hello");
        let b = arena.intern("world");

        unsafe {
            assert_eq!(arena.as_str(a), "hello");
            assert_eq!(arena.as_str(b), "world");
        }
    }

    #[test]
    fn interning_has_equal_pointer() {
        let mut arena = UnsafeArena::default();
        let a = arena.intern("hello");
        let b = arena.intern("hello");

        unsafe {
            let a_str = arena.as_str(a);
            let b_str = arena.as_str(b);

            assert_eq!(a_str, b_str);
            assert!(std::ptr::eq(a_str, b_str));
        }
    }

    fn highly_duplicated<T>() -> impl Strategy<Value = Vec<T>>
    where
        T: Arbitrary + Clone,
    {
        any::<Vec<T>>().prop_perturb(|mut v, mut rng| {
            if v.is_empty() {
                return v;
            }

            let n_dupes = rng.gen_range(0..v.len());
            let dupes: Vec<_> = v.choose_multiple(&mut rng, n_dupes).cloned().collect();
            v.extend(dupes);
            v.shuffle(&mut rng);
            v
        })
    }

    static CONFIG: LazyLock<ProptestConfig> = LazyLock::new(|| {
        let mut cfg = ProptestConfig::default();

        if cfg!(miri) {
            cfg.cases = 1;
            cfg.failure_persistence = None;
        }

        cfg
    });

    proptest! {
        #![proptest_config(CONFIG)]

        #[test]
        fn all_interned_keys_equal_each_other(s in highly_duplicated::<String>()) {
            let mut arena = UnsafeArena::default();
            let mut by_string = HashMap::with_capacity(s.len());

            for s in s {
                let k = arena.intern(&s);
                by_string.entry(s).or_insert_with(Vec::new).push(k);
            }

            for (s, keys) in &by_string {
                let (first_key, keys) = keys.split_first().unwrap();

                assert!(
                    keys.iter().all(|k| first_key == k),
                    "Not all interned keys for {} were equal: {:?}",
                    s,
                    keys,
                );

                unsafe {
                    assert_eq!(s, arena.as_str(*first_key));
                }
            }
        }
    }
}
