#[easy_ext::ext(StrExt)]
pub impl str {
    #[inline]
    fn nc_name(&self) -> (&str, &str) {
        let mut n_valid = 0;
        let mut chars = self.chars();

        if let Some(c) = chars.next() {
            if c.is_name_start_char() {
                n_valid += c.len_utf8();

                for c in chars {
                    if c.is_name_char() {
                        n_valid += c.len_utf8();
                    } else {
                        break;
                    }
                }
            }
        }

        self.split_at(n_valid)
    }
}

#[easy_ext::ext(SliceExt)]
pub impl [u8] {
    #[inline]
    fn nc_name(&self) -> (&str, &[u8]) {
        let s = match std::str::from_utf8(self) {
            Ok(s) => s,
            Err(e) => {
                let valid_part = &self[..e.valid_up_to()];
                // SAFETY: We just validated this part of the slice
                unsafe { std::str::from_utf8_unchecked(valid_part) }
            }
        };

        let (name, _) = s.nc_name();
        let rest = &self[name.len()..];
        (name, rest)
    }
}

#[easy_ext::ext]
impl char {
    // ignoring ":"
    #[allow(clippy::wrong_self_convention)]
    fn is_name_start_char(self) -> bool {
        matches!(
            self,
            'A'..='Z'
                | '_'
                | 'a'..='z'
                | '\u{C0}'..='\u{D6}'
                | '\u{D8}'..='\u{F6}'
                | '\u{F8}'..='\u{2FF}'
                | '\u{370}'..='\u{37D}'
                | '\u{37F}'..='\u{1FFF}'
                | '\u{200C}'..='\u{200D}'
                | '\u{2070}'..='\u{218F}'
                | '\u{2C00}'..='\u{2FEF}'
                | '\u{3001}'..='\u{D7FF}'
                | '\u{F900}'..='\u{FDCF}'
                | '\u{FDF0}'..='\u{FFFD}'
                | '\u{10000}'..='\u{EFFFF}'
        )
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_name_cont_char(self) -> bool {
        matches!(
            self,
            '-'
                | '.'
                | '0'..='9'
                | '\u{B7}'
                | '\u{0300}'..='\u{036F}'
                | '\u{203F}'..='\u{2040}'
        )
    }

    #[allow(clippy::wrong_self_convention)]
    fn is_name_char(self) -> bool {
        self.is_name_start_char() || self.is_name_cont_char()
    }
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;

    use super::*;

    proptest! {
        #[test]
        fn matches_naive_implementation(b in prop::array::uniform::<_, 12>(prop::num::u8::ANY)) {
            matches_naive_implementation_impl(b)?;
        }
    }

    fn matches_naive_implementation_impl(bytes: impl AsRef<[u8]>) -> Result<(), TestCaseError> {
        let bytes = bytes.as_ref();

        let naive_result = crate::SliceExt::nc_name(bytes);
        let our_result = bytes.nc_name();

        prop_assert_eq!(naive_result, our_result);
        Ok(())
    }
}
