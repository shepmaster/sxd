#![deny(rust_2018_idioms)]

pub use libxml2_sys;
pub use sxd_pull_parser;
pub use sxd_validation;

use sxd_pull_parser::Parser;
use sxd_validation::Validator;

pub type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

pub fn parse(s: &str) -> Result<usize> {
    let mut parser = Validator::new(Parser::new(s.as_bytes()));
    let mut count = 0;

    while let Some(v) = parser.next_str() {
        v?;
        count += 1;
    }

    Ok(count)
}

pub fn assert_both_parse(data: &[u8]) {
    if let Ok(s) = std::str::from_utf8(data) {
        assert_both_parse_str(s);
    }
}

pub fn assert_both_parse_str(s: &str) {
    let lx = match libxml2_sys::parse(s) {
        Ok(lx) => lx,
        Err(_) => return,
    };

    if let Err(e) = parse(s) {
        panic!("libxml2 parsed {s:?} as {lx}, but we failed due to {e} / {e:?}");
    }
}

pub fn assert_both_fail(data: &[u8]) {
    if let Ok(s) = std::str::from_utf8(data) {
        assert_both_fail_str(s);
    }
}

pub fn assert_both_fail_str(s: &str) {
    let lx = match libxml2_sys::parse(s) {
        Ok(_) => return,
        Err(e) => e,
    };

    if parse(s).is_ok() {
        panic!("libxml2 failed to parse {s:?} due to {lx:?}, but we succeeded");
    }
}
