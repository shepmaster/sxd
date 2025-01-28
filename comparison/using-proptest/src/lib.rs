#![deny(rust_2018_idioms)]
#![cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn parses_everything_libxml2_does(d: Vec<u8>) {
        comparison::assert_both_parse(&d);
    }
}
