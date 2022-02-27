#![deny(rust_2018_idioms)]

use std::fmt;

include!(concat!(env!("OUT_DIR"), "/generated_token.rs"));

pub type UniformToken<T> = Token<UniformKind<T>>;

pub trait Source {
    type IndexKind: TokenKind;
    type StrKind<'a>: TokenKind
    where
        Self: 'a;
    type Error: std::error::Error + 'static;

    // This method (and similar methods that return `usize` or other
    // non-reference types) are a workaround for the current
    // limitations of the borrow checker. If Polonius is ever merged,
    // this can be simplified.
    fn next_index(&mut self) -> Option<Result<Token<Self::IndexKind>, Self::Error>>;

    fn next_str(&mut self) -> Option<Result<Token<Self::StrKind<'_>>, Self::Error>>;
}

pub trait IsComplete {
    fn is_complete(&self) -> bool;
}

impl<T> IsComplete for &'_ T
where
    T: ?Sized + IsComplete,
{
    #[inline]
    fn is_complete(&self) -> bool {
        T::is_complete(self)
    }
}

impl IsComplete for str {
    #[inline]
    fn is_complete(&self) -> bool {
        true
    }
}

#[derive(Debug, Copy, Clone, Eq)]
pub enum Streaming<T> {
    Partial(T),
    Complete(T),
}

impl<T> Streaming<T> {
    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Streaming<U> {
        use Streaming::*;

        match self {
            Partial(v) => Partial(f(v)),
            Complete(v) => Complete(f(v)),
        }
    }

    #[inline]
    pub fn unify(&self) -> &T {
        use Streaming::*;

        match self {
            Partial(a) => a,
            Complete(a) => a,
        }
    }
}

impl<T> IsComplete for Streaming<T> {
    #[inline]
    fn is_complete(&self) -> bool {
        matches!(self, Streaming::Complete(_))
    }
}

impl<T> AsRef<T> for Streaming<T> {
    fn as_ref(&self) -> &T {
        self.unify()
    }
}

impl AsRef<str> for Streaming<&str> {
    fn as_ref(&self) -> &str {
        self.unify()
    }
}

impl<T, U> PartialEq<Streaming<U>> for Streaming<T>
where
    T: PartialEq<U>,
{
    #[inline]
    fn eq(&self, other: &Streaming<U>) -> bool {
        match (self, other) {
            (Streaming::Partial(a), Streaming::Partial(b)) => a == b,
            (Streaming::Complete(a), Streaming::Complete(b)) => a == b,
            _ => false,
        }
    }
}

impl<T> fmt::Display for Streaming<T>
where
    T: fmt::Display,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.unify().fmt(f)
    }
}

/// Many producers of tokens return an "index" — a value which doesn't
/// contain a reference. Consumers of tokens want to work with a
/// `&str` (raw or wrapped). This allows exchanging the index for a
/// string-like type.
pub trait Exchange<I> {
    type Output<'a>
    where
        Self: 'a;

    fn exchange(&self, idx: I) -> Self::Output<'_>;
}
