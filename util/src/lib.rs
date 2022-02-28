use std::mem;

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

    pub fn as_ref(&self) -> QName<&T> {
        QName {
            prefix: self.prefix.as_ref(),
            local_part: &self.local_part,
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

impl<T> PartialEq<QName<T>> for str
where
    T: AsRef<str>,
{
    fn eq(&self, other: &QName<T>) -> bool {
        other == self
    }
}

impl<T> PartialEq<QName<T>> for &str
where
    T: AsRef<str>,
{
    fn eq(&self, other: &QName<T>) -> bool {
        other == *self
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

impl<'a> From<&'a str> for QName<&'a str> {
    fn from(other: &'a str) -> Self {
        Self {
            prefix: None,
            local_part: other,
        }
    }
}

impl<'a> From<&'a String> for QName<&'a str> {
    fn from(other: &'a String) -> Self {
        Self {
            prefix: None,
            local_part: other,
        }
    }
}

impl From<String> for QName<String> {
    fn from(other: String) -> Self {
        Self {
            prefix: None,
            local_part: other,
        }
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

#[derive(Debug)]
pub struct QNameBuilder2<T>([Option<T>; 2]);

impl<T> Default for QNameBuilder2<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T> QNameBuilder2<T> {
    pub fn push(&mut self, v: T) {
        self.0.rotate_left(1);
        self.0[1] = Some(v);
    }

    pub fn finish(&mut self) -> Option<QName<T>> {
        match mem::take(&mut self.0) {
            [prefix, Some(local_part)] => Some(QName { prefix, local_part }),
            [_, None] => None,
        }
    }
}
