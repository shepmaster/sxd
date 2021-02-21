use std::fmt;

macro_rules! impl_token {
    (
        $(#[$o_meta:meta])*
        $vis:vis enum $e_name:ident<$g:ident> {
            $(
                $(#[$i_meta:meta])*
                $v_name:ident $(($field:ident))?
            ,)*
        }
    ) => {
        $(#[$o_meta])*
        $vis enum $e_name<$g> {
            $($(#[$i_meta])* $v_name $(($field))?,)*
        }

        impl<$g> $e_name<$g> {
            #[allow(non_snake_case)]
            #[inline]
            pub fn map<U>(self, f: impl FnOnce($g) -> U) ->  $e_name<U> {
                use $e_name::*;

                match self {
                    $($v_name $(($field))? => $v_name $((f($field)))?, )*
                }
            }
        }

        impl<$g, U> PartialEq<$e_name<U>> for $e_name<$g>
        where
            $g: PartialEq<U>,
        {
            #[inline]
            fn eq(&self, other: &$e_name<U>) -> bool {
                use $e_name::*;

                match (self, other) {
                    $(impl_token!(@eq_pat s1 s2 $v_name $($field)?) =>
                      impl_token!(@eq_arm s1 s2 $v_name $($field)?) ,)*
                    _ => false,
                }
            }
        }
    };

    (@eq_pat $s1:ident $s2:ident $v_name:ident $field:ty) => { ($v_name($s1), $v_name($s2)) };
    (@eq_arm $s1:ident $s2:ident $v_name:ident $field:ty) => { $s1 == $s2 };
    (@eq_pat $s1:ident $s2:ident $v_name:ident) => { ($v_name, $v_name) };
    (@eq_arm $s1:ident $s2:ident $v_name:ident) => { true };
}

impl_token! {
    #[derive(Debug, Copy, Clone, Eq)]
    pub enum Token<T> {
        /// `<?xml version="1.9"`
        DeclarationStart(T),
        /// `?>`
        DeclarationClose,

        /// `<foo`
        ElementOpenStart(T),
        /// `>`
        ElementOpenEnd,
        /// `/>`
        ElementSelfClose,

        /// `</foo`
        ElementClose(T),

        /// `foo`
        AttributeName(T),
        /// `="bar`
        AttributeValue(T),

        /// `hello world`
        CharData(T),
        /// `<![CDATA[hello world]]>`
        CData(T),
        Space(T),

        /// &lt;
        ReferenceNamed(T),
        /// &#4242;
        ReferenceDecimal(T),
        /// &#xABCD;
        ReferenceHex(T),

        /// `<?a`
        ProcessingInstructionStart(T),
        /// `b`
        ProcessingInstructionValue(T),
        /// `?>`
        ProcessingInstructionEnd,

        /// `<!--a-->`
        Comment(T),
    }
}

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
