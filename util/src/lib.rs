use std::ops::Deref;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct QName<T> {
    pub prefix: Option<T>,
    pub local_part: T,
}

impl<T> QName<T> {
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> QName<U> {
        QName {
            prefix: self.prefix.map(&mut f),
            local_part: f(self.local_part),
        }
    }

    pub fn as_deref(&self) -> QName<&T::Target>
    where
        T: Deref,
    {
        QName {
            prefix: self.prefix.as_deref(),
            local_part: &*self.local_part,
        }
    }
}

impl<T> PartialEq<str> for QName<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &str) -> bool {
        self.prefix.is_none() && self.local_part.as_ref() == other
    }
}

impl<T> PartialEq<(&str, &str)> for QName<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &(&str, &str)) -> bool {
        self.prefix.as_ref().map(AsRef::as_ref) == Some(other.0)
            && self.local_part.as_ref() == other.1
    }
}

impl<T> PartialEq<(&str, &str)> for &QName<T>
where
    T: AsRef<str>,
{
    fn eq(&self, other: &(&str, &str)) -> bool {
        *self == other
    }
}

impl From<QName<&str>> for QName<String> {
    fn from(other: QName<&str>) -> Self {
        other.map(From::from)
    }
}

impl<T> From<(T, T)> for QName<T> {
    fn from(other: (T, T)) -> Self {
        Self {
            prefix: Some(other.0),
            local_part: other.1,
        }
    }
}

#[derive(Debug)]
pub struct QNameBuilder<T>(T);

impl<T> QNameBuilder<T> {
    pub fn new(value: T) -> Self {
        Self(value)
    }

    pub fn finish(self) -> QName<T> {
        QName {
            prefix: None,
            local_part: self.0,
        }
    }

    pub fn push(self, local_part: T) -> QName<T> {
        QName {
            prefix: Some(self.0),
            local_part,
        }
    }
}
