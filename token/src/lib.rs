use std::fmt;

include!(concat!(env!("OUT_DIR"), "/generated_token.rs"));

pub type UniformToken<T> = Token<UniformKind<T>>;

#[derive(Debug, Copy, Clone, Eq)]
pub enum Streaming<T> {
    Partial(T),
    Complete(T),
}

impl<T> Streaming<T> {
    #[inline]
    pub fn is_complete(&self) -> bool {
        matches!(self, Streaming::Complete(_))
    }

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
