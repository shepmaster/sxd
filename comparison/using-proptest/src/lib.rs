#![cfg(test)]

use proptest::prelude::*;

proptest! {
    #[test]
    fn parses_everything_libxml2_does(s: String) {
        if let Ok(lx) = libxml2_sys::parse(&s) {
            if let Err(e) = comparison::parse(&s) {
                panic!(
                    "libxml2 parsed {:?} as {}, but we couldn't ({})",
                    s,
                    lx,
                    e,
                );
            }
        }
    }
}
