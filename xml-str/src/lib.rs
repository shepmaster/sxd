#![cfg_attr(not(test), no_std)]

use core::{ops, str};

mod sealed {
    pub trait Sealed {}

    impl Sealed for u8 {}
    impl Sealed for char {}
    impl Sealed for str {}
    impl Sealed for [u8] {}
}

pub trait U8Ext: sealed::Sealed {
    #[must_use]
    fn is_xml_space(&self) -> bool;
}

impl U8Ext for u8 {
    #[inline]
    fn is_xml_space(&self) -> bool {
        matches!(*self, b' ' | b'\t' | b'\r' | b'\n')
    }
}

pub trait CharExt: sealed::Sealed {
    #[must_use]
    fn is_xml_char(&self) -> bool;
}

impl CharExt for char {
    #[inline]
    fn is_xml_char(&self) -> bool {
        // Sorted by how common each case is which noticeably impacts
        // performance
        matches!(
            self,
                '\u{20}'..='\u{FF}'
                | '\u{9}'
                | '\u{A}'
                | '\u{D}'
                | '\u{100}'..='\u{D7FF}'
                | '\u{E000}'..='\u{FFFD}'
                | '\u{10000}'..='\u{10FFFF}'
        )
    }
}

pub trait StrExt: sealed::Sealed {
    #[must_use]
    fn is_xml_space(&self) -> bool;
}

impl StrExt for str {
    #[inline]
    fn is_xml_space(&self) -> bool {
        let (_, r) = self.as_bytes().xml_space();
        r.is_empty()
    }
}

pub trait SliceExt: sealed::Sealed {
    #[must_use]
    fn enough_to_parse(&self) -> bool;

    #[must_use]
    fn xml_space(&self) -> (&str, &[u8]);

    #[must_use]
    fn xml_chars(&self) -> (&str, &[u8]);

    #[must_use]
    fn char_data(&self) -> (&str, &[u8]);

    #[must_use]
    fn reference_decimal(&self) -> (&str, &[u8]);

    #[must_use]
    fn reference_hex(&self) -> (&str, &[u8]);

    #[must_use]
    fn nc_name(&self) -> (&str, &[u8]);

    #[must_use]
    fn nc_name_continuation(&self) -> (&str, &[u8]);
}

impl SliceExt for [u8] {
    #[inline]
    fn enough_to_parse(&self) -> bool {
        match self.first() {
            Some(&b) => self.len() >= utf8_char_width(b),
            None => false,
        }
    }

    #[inline]
    fn xml_space(&self) -> (&str, &[u8]) {
        let i = self.xml_space_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn xml_chars(&self) -> (&str, &[u8]) {
        let i = self.xml_chars_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn char_data(&self) -> (&str, &[u8]) {
        let i = self.char_data_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn reference_decimal(&self) -> (&str, &[u8]) {
        let i = self.reference_decimal_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn reference_hex(&self) -> (&str, &[u8]) {
        let i = self.reference_hex_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn nc_name(&self) -> (&str, &[u8]) {
        let i = self.nc_name_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }

    #[inline]
    fn nc_name_continuation(&self) -> (&str, &[u8]) {
        let i = self.nc_name_continuation_index();

        // SAFETY: `SliceIndexExt` guarantees the index is in bounds and valid UTF-8.
        unsafe { self.split_leading_str(i) }
    }
}

/// # Safety
///
/// We rely on this to disallow invalid UTF-8. Implementors must
/// guarantee to validate the bytes. Implementors do not need to worry
/// about validating continuation bytes.
unsafe trait Utf8ByteValidation {
    fn is_valid_1(&self, byte: u8) -> bool;
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool;
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool;
    fn is_valid_4(&self, bytes: [u8; 4]) -> bool;
}

#[must_use]
#[inline]
const fn encode_codepoint<const N: usize>(cp: u32) -> [u8; N] {
    let c = char::from_u32(cp).unwrap();
    let mut b = [0; N];
    c.encode_utf8(&mut b);
    b
}

#[must_use]
#[inline]
const fn range<const N: usize>(codepoints: ops::RangeInclusive<u32>) -> [[u8; N]; 2] {
    [
        encode_codepoint(*codepoints.start()),
        encode_codepoint(*codepoints.end()),
    ]
}

#[must_use]
#[inline]
fn in_range<const NB: usize, const NR: usize>(
    bytes: [u8; NB],
    ranges: [[[u8; NB]; 2]; NR],
) -> bool {
    ranges.into_iter().any(|[s, e]| s <= bytes && bytes <= e)
}

struct XmlSpace;

// SAFETY: XML spaces are a subset of ASCII, which is a subset of UTF-8.
unsafe impl Utf8ByteValidation for XmlSpace {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        byte.is_xml_space()
    }

    #[inline]
    fn is_valid_2(&self, _bytes: [u8; 2]) -> bool {
        false
    }

    #[inline]
    fn is_valid_3(&self, _bytes: [u8; 3]) -> bool {
        false
    }

    #[inline]
    fn is_valid_4(&self, _bytes: [u8; 4]) -> bool {
        false
    }
}

struct XmlChar;

// SAFETY: The ranges we validate are a subset of UTF-8.
unsafe impl Utf8ByteValidation for XmlChar {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        matches!(byte, 0x9 | 0xA | 0xD | 0x20..=0x7F)
    }

    #[inline]
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool {
        let ranges = [range(0x80..=0x07ff)];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool {
        let ranges = [range(0x0800..=0xd7ff), range(0xe000..=0xfffd)];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_4(&self, bytes: [u8; 4]) -> bool {
        let ranges = [range(0x10000..=0x10ffff)];

        in_range(bytes, ranges)
    }
}

struct CharData;

// SAFETY: The ranges we validate are a subset of UTF-8.
unsafe impl Utf8ByteValidation for CharData {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        if matches!(byte, b'<' | b'&' | b']') {
            return false;
        }

        XmlChar.is_valid_1(byte)
    }

    #[inline]
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool {
        XmlChar.is_valid_2(bytes)
    }

    #[inline]
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool {
        XmlChar.is_valid_3(bytes)
    }

    #[inline]
    fn is_valid_4(&self, bytes: [u8; 4]) -> bool {
        XmlChar.is_valid_4(bytes)
    }
}

struct ReferenceDecimal;

// SAFETY: ASCII digits are a subset of ASCII, which is a subset of UTF-8.
unsafe impl Utf8ByteValidation for ReferenceDecimal {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        byte.is_ascii_digit()
    }

    #[inline]
    fn is_valid_2(&self, _bytes: [u8; 2]) -> bool {
        false
    }

    #[inline]
    fn is_valid_3(&self, _bytes: [u8; 3]) -> bool {
        false
    }

    #[inline]
    fn is_valid_4(&self, _bytes: [u8; 4]) -> bool {
        false
    }
}

struct ReferenceHex;

// SAFETY: ASCII hexdigits are a subset of ASCII, which is a subset of UTF-8.
unsafe impl Utf8ByteValidation for ReferenceHex {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        byte.is_ascii_hexdigit()
    }

    #[inline]
    fn is_valid_2(&self, _bytes: [u8; 2]) -> bool {
        false
    }

    #[inline]
    fn is_valid_3(&self, _bytes: [u8; 3]) -> bool {
        false
    }

    #[inline]
    fn is_valid_4(&self, _bytes: [u8; 4]) -> bool {
        false
    }
}

struct NCNameStart;

// SAFETY: The ranges we validate are a subset of UTF-8.
unsafe impl Utf8ByteValidation for NCNameStart {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        matches!(byte, b'A'..=b'Z' | b'_' | b'a'..=b'z')
    }

    #[inline]
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool {
        let ranges = [
            range(0xc0..=0xd6),
            range(0xd8..=0xf6),
            range(0xf8..=0x2ff),
            range(0x370..=0x37d),
            range(0x37f..=0x7ff),
        ];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool {
        let ranges = [
            range(0x800..=0x1fff),
            range(0x200c..=0x200d),
            range(0x2070..=0x218f),
            range(0x2c00..=0x2fef),
            range(0x3001..=0xd7ff),
            range(0xf900..=0xfdcf),
            range(0xfdf0..=0xfffd),
        ];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_4(&self, bytes: [u8; 4]) -> bool {
        let ranges = [range(0x10000..=0xeffff)];

        in_range(bytes, ranges)
    }
}

struct NCNameContinuationOnly;

// SAFETY: The ranges we validate are a subset of UTF-8.
unsafe impl Utf8ByteValidation for NCNameContinuationOnly {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        matches!(byte, b'-' | b'.' | b'0'..=b'9')
    }

    #[inline]
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool {
        let ranges = [range(0xb7..=0xb7), range(0x300..=0x36f)];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool {
        let ranges = [range(0x203f..=0x2040)];

        in_range(bytes, ranges)
    }

    #[inline]
    fn is_valid_4(&self, _bytes: [u8; 4]) -> bool {
        false
    }
}

struct NCName;

// SAFETY: We call `NCNameStart` and `NCNameContinuationOnly` and
// union the results. Neither of those should allow invalid
// characters, so neither should we.
unsafe impl Utf8ByteValidation for NCName {
    #[inline]
    fn is_valid_1(&self, byte: u8) -> bool {
        NCNameStart.is_valid_1(byte) || NCNameContinuationOnly.is_valid_1(byte)
    }

    #[inline]
    fn is_valid_2(&self, bytes: [u8; 2]) -> bool {
        NCNameStart.is_valid_2(bytes) || NCNameContinuationOnly.is_valid_2(bytes)
    }

    #[inline]
    fn is_valid_3(&self, bytes: [u8; 3]) -> bool {
        NCNameStart.is_valid_3(bytes) || NCNameContinuationOnly.is_valid_3(bytes)
    }

    #[inline]
    fn is_valid_4(&self, bytes: [u8; 4]) -> bool {
        NCNameStart.is_valid_4(bytes) || NCNameContinuationOnly.is_valid_4(bytes)
    }
}

trait InternalSliceExt: sealed::Sealed {
    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn xml_space_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn xml_chars_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn char_data_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn reference_decimal_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn reference_hex_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn nc_name_index(&self) -> usize;

    /// # Guarantees
    ///
    /// The index is in bounds and the leading bytes are a (subset of) UTF-8.
    #[must_use]
    fn nc_name_continuation_index(&self) -> usize;

    #[must_use]
    fn find_split_point(&self, i: usize, v: impl Utf8ByteValidation) -> usize;

    /// # Safety
    ///
    /// The index must be in bounds of the slice. All bytes up to the index must be valid UTF-8.
    #[must_use]
    unsafe fn split_leading_str(&self, i: usize) -> (&str, &[u8]);

    #[must_use]
    fn get_cont_byte(&self, i: usize) -> Option<u8>;
}

impl InternalSliceExt for [u8] {
    #[inline]
    fn xml_space_index(&self) -> usize {
        self.find_split_point(0, XmlSpace)
    }

    #[inline]
    fn xml_chars_index(&self) -> usize {
        self.find_split_point(0, XmlChar)
    }

    #[inline]
    fn char_data_index(&self) -> usize {
        let mut i = 0;

        loop {
            i = self.find_split_point(i, CharData);

            match self.get(i) {
                Some(b']') => {
                    match self.get(i + 1) {
                        Some(b']') => {
                            match self.get(i + 2) {
                                // We found `]]>`
                                Some(b'>') => break,

                                // We know it's definitely not `]]>`
                                Some(_) => {
                                    // Only advance by one byte in case we are parsing `]]]>`
                                    i += 1;
                                    continue;
                                }

                                // We ran out of bytes and can't tell
                                _ => break,
                            }
                        }

                        // We know it's definitely not `]]`
                        Some(_) => {
                            i += 1;
                            continue;
                        }

                        // We ran out of bytes and can't tell
                        _ => break,
                    }
                }

                // It's `<`, `&`, or we ran out of bytes
                _ => break,
            }
        }

        i
    }

    #[inline]
    fn reference_decimal_index(&self) -> usize {
        self.find_split_point(0, ReferenceDecimal)
    }

    #[inline]
    fn reference_hex_index(&self) -> usize {
        self.find_split_point(0, ReferenceHex)
    }

    #[inline]
    fn nc_name_index(&self) -> usize {
        let mut i = self.find_split_point(0, NCNameStart);
        if i != 0 {
            i = self.find_split_point(i, NCName);
        }

        i
    }

    #[inline]
    fn nc_name_continuation_index(&self) -> usize {
        self.find_split_point(0, NCName)
    }

    #[inline]
    fn find_split_point(&self, mut i: usize, v: impl Utf8ByteValidation) -> usize {
        while let Some(&b0) = self.get(i) {
            if b0 < 0b1000_0000 {
                // ASCII

                if !v.is_valid_1(b0) {
                    break;
                }

                i += 1;
            } else {
                let width = utf8_char_width(b0);

                macro_rules! fetch {
                    ($i:expr) => {
                        match self.get_cont_byte($i) {
                            Some(b) => b,
                            None => break,
                        }
                    };
                }

                match width {
                    2 => {
                        let b1 = fetch!(i + 1);

                        if !v.is_valid_2([b0, b1]) {
                            break;
                        }
                    }

                    3 => {
                        let b1 = fetch!(i + 1);
                        let b2 = fetch!(i + 2);

                        if !v.is_valid_3([b0, b1, b2]) {
                            break;
                        }
                    }

                    4 => {
                        let b1 = fetch!(i + 1);
                        let b2 = fetch!(i + 2);
                        let b3 = fetch!(i + 3);

                        if !v.is_valid_4([b0, b1, b2, b3]) {
                            break;
                        }
                    }

                    _ => break,
                }

                i += width;
            }
        }

        i
    }

    /// # Safety
    ///
    /// The index must be in bounds of the slice. The bytes up to the index must be valid UTF-8.
    #[inline]
    unsafe fn split_leading_str(&self, i: usize) -> (&str, &[u8]) {
        // SAFETY: guaranteed by the caller
        unsafe {
            let (name, rest) = self.split_at_unchecked(i);
            let name = str::from_utf8_unchecked(name);
            (name, rest)
        }
    }

    #[inline]
    fn get_cont_byte(&self, i: usize) -> Option<u8> {
        self.get(i).copied().filter(|&b| utf8_is_cont_byte(b))
    }
}

// https://tools.ietf.org/html/rfc3629
const UTF8_CHAR_WIDTH: &[u8; 256] = &[
    // 1  2  3  4  5  6  7  8  9  A  B  C  D  E  F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 3
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 4
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 5
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 6
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 7
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 9
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // A
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // B
    0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // D
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // E
    4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // F
];

/// Given a first byte, determines how many bytes are in this UTF-8 character.
// Copied from `core::str::utf8_char_width`
#[must_use]
#[inline]
const fn utf8_char_width(b: u8) -> usize {
    UTF8_CHAR_WIDTH[b as usize] as usize
}

#[must_use]
#[inline]
const fn utf8_is_cont_byte(byte: u8) -> bool {
    matches!(byte, 0x80..=0xBF)
}

#[cfg(test)]
mod test {
    use proptest::prelude::*;

    use super::*;

    fn some_bytes() -> impl Strategy<Value = [u8; 12]> {
        prop::array::uniform::<_, 12>(prop::num::u8::ANY)
    }

    proptest! {
        #[test]
        fn xml_space_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::xml_space_index, b)?;
        }

        #[test]
        fn xml_chars_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::xml_chars_index, b)?;
        }

        #[test]
        fn char_data_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::char_data_index, b)?;
        }

        #[test]
        fn reference_decimal_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::reference_decimal_index, b)?;
        }

        #[test]
        fn reference_hex_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::reference_hex_index, b)?;
        }

        #[test]
        fn nc_name_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::nc_name_index, b)?;
        }

        #[test]
        fn nc_name_continuation_is_valid(b in some_bytes()) {
            validate_index(<[u8]>::nc_name_continuation_index, b)?;
        }
    }

    #[cfg(test)]
    fn validate_index(
        f: impl Fn(&[u8]) -> usize,
        bytes: impl AsRef<[u8]>,
    ) -> Result<(), TestCaseError> {
        let bytes = bytes.as_ref();

        let i = f(bytes);

        // Checks that `i` is in bounds
        let (head, tail) = bytes.split_at(i);

        let head = str::from_utf8(head).expect("Must always be UTF-8");

        prop_assert_eq!(head.len() + tail.len(), bytes.len());

        Ok(())
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_xml_space() {
        bytes_that_never_appear_in_utf_8(<[u8]>::xml_space);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_xml_chars() {
        bytes_that_never_appear_in_utf_8(<[u8]>::xml_chars);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_char_data() {
        bytes_that_never_appear_in_utf_8(<[u8]>::char_data);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_reference_decimal() {
        bytes_that_never_appear_in_utf_8(<[u8]>::reference_decimal);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_reference_hex() {
        bytes_that_never_appear_in_utf_8(<[u8]>::reference_hex);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_nc_name_continuation() {
        bytes_that_never_appear_in_utf_8(<[u8]>::nc_name_continuation);
    }

    #[test]
    fn bytes_that_never_appear_in_utf_8_nc_name() {
        bytes_that_never_appear_in_utf_8(<[u8]>::nc_name);
    }

    #[track_caller]
    fn bytes_that_never_appear_in_utf_8(i: impl Fn(&[u8]) -> (&str, &[u8])) {
        for b in never_utf8_bytes() {
            let bytes = &[b];
            let (head, tail) = i(bytes);
            assert_eq!(head, "");
            assert_eq!(tail, bytes);
        }
    }

    #[test]
    fn continuation_byte_at_the_start_of_a_character() {
        for cb in continuation_bytes() {
            let mut hello = *b"hello";
            hello[0] = cb;
            let i = hello.find_split_point(0, UnreachableValidation);
            assert_eq!(i, 0);
        }
    }

    #[test]
    fn non_continuation_byte_before_the_end_of_a_character() {
        let omega = "Ω";
        let omega_bytes: [u8; 2] = omega.as_bytes().try_into().unwrap();

        let euro = "€";
        let euro_bytes: [u8; 3] = euro.as_bytes().try_into().unwrap();

        let bug = "🐛";
        let bug_bytes: [u8; 4] = bug.as_bytes().try_into().unwrap();

        for ncb in non_continuation_bytes() {
            for i in 1..2 {
                let mut bytes = omega_bytes;
                bytes[i] = ncb;

                let i = bytes.find_split_point(0, UnreachableValidation);
                assert_eq!(i, 0);
            }

            for i in 1..3 {
                let mut bytes = euro_bytes;
                bytes[i] = ncb;

                let i = bytes.find_split_point(0, UnreachableValidation);
                assert_eq!(i, 0);
            }

            for i in 1..4 {
                let mut bytes = bug_bytes;
                bytes[i] = ncb;

                let i = bytes.find_split_point(0, UnreachableValidation);
                assert_eq!(i, 0);
            }
        }
    }

    #[test]
    fn overlong_encoding_xml_space() {
        overlong_encoding(<[u8]>::xml_space);
    }

    #[test]
    fn overlong_encoding_xml_chars() {
        overlong_encoding(<[u8]>::xml_chars);
    }

    #[test]
    fn overlong_encoding_char_data() {
        overlong_encoding(<[u8]>::char_data);
    }

    #[test]
    fn overlong_encoding_reference_decimal() {
        overlong_encoding(<[u8]>::reference_decimal);
    }

    #[test]
    fn overlong_encoding_reference_hex() {
        overlong_encoding(<[u8]>::reference_hex);
    }

    #[test]
    fn overlong_encoding_nc_name_continuation() {
        overlong_encoding(<[u8]>::nc_name_continuation);
    }

    #[test]
    fn overlong_encoding_nc_name() {
        overlong_encoding(<[u8]>::nc_name);
    }

    #[track_caller]
    fn overlong_encoding(i: impl Fn(&[u8]) -> (&str, &[u8])) {
        let bytes = [0xC0, 0xA0];
        let codepoint = decode_2_bytes(bytes);
        assert_eq!(codepoint, b' '.into());

        let (head, tail) = i(&bytes);
        assert_eq!(head, "");
        assert_eq!(tail, &bytes);

        let bytes = [0xE0, 0x80, 0xA0];
        let codepoint = decode_3_bytes(bytes);
        assert_eq!(codepoint, b' '.into());

        let (head, tail) = i(&bytes);
        assert_eq!(head, "");
        assert_eq!(tail, &bytes);

        let bytes = [0xF0, 0x80, 0x80, 0xA0];
        let codepoint = decode_4_bytes(bytes);
        assert_eq!(codepoint, b' '.into());

        let (head, tail) = i(&bytes);
        assert_eq!(head, "");
        assert_eq!(tail, &bytes);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_xml_space() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::xml_space);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_xml_chars() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::xml_chars);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_char_data() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::char_data);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_reference_decimal() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::reference_decimal);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_reference_hex() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::reference_hex);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_nc_name_continuation() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::nc_name_continuation);
    }

    #[test]
    fn decodes_to_a_value_greater_than_maximum_nc_name() {
        decodes_to_a_value_greater_than_maximum(<[u8]>::nc_name);
    }

    #[track_caller]
    fn decodes_to_a_value_greater_than_maximum(i: impl Fn(&[u8]) -> (&str, &[u8])) {
        let bytes = [0xF4, 0x90, 0x80, 0x80];
        assert!(decode_4_bytes(bytes) > 0x10FFFF);

        let (head, tail) = i(&bytes);
        assert_eq!(head, "");
        assert_eq!(tail, &bytes);
    }

    // --------------------

    fn never_utf8_bytes() -> impl Iterator<Item = u8> {
        (0xC0..=0xC1).chain(0xF5..=0xFF)
    }

    fn continuation_bytes() -> impl Iterator<Item = u8> {
        (0..=0xFF).filter(|&b| utf8_is_cont_byte(b))
    }

    fn non_continuation_bytes() -> impl Iterator<Item = u8> {
        (0..=0xFF).filter(|&b| !utf8_is_cont_byte(b))
    }

    fn decode_2_bytes(bytes: [u8; 2]) -> u32 {
        let a = bytes[0] & 0b11111;
        let b = bytes[1] & 0b111111;

        let a = u32::from(a);
        let b = u32::from(b);

        a << 6 | b
    }

    fn decode_3_bytes(bytes: [u8; 3]) -> u32 {
        let a = bytes[0] & 0b1111;
        let b = bytes[1] & 0b111111;
        let c = bytes[2] & 0b111111;

        let a = u32::from(a);
        let b = u32::from(b);
        let c = u32::from(c);

        a << 12 | b << 6 | c
    }

    fn decode_4_bytes(bytes: [u8; 4]) -> u32 {
        let a = bytes[0] & 0b111;
        let b = bytes[1] & 0b111111;
        let c = bytes[2] & 0b111111;
        let d = bytes[3] & 0b111111;

        let a = u32::from(a);
        let b = u32::from(b);
        let c = u32::from(c);
        let d = u32::from(d);

        a << 18 | b << 12 | c << 6 | d
    }

    struct UnreachableValidation;

    // SAFETY: Code always panics.
    unsafe impl Utf8ByteValidation for UnreachableValidation {
        fn is_valid_1(&self, _byte: u8) -> bool {
            unreachable!()
        }

        fn is_valid_2(&self, _bytes: [u8; 2]) -> bool {
            unreachable!()
        }

        fn is_valid_3(&self, _bytes: [u8; 3]) -> bool {
            unreachable!()
        }

        fn is_valid_4(&self, _bytes: [u8; 4]) -> bool {
            unreachable!()
        }
    }
}
