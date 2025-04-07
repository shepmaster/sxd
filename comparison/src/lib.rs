#![deny(rust_2018_idioms)]

pub use libxml2_sys;
pub use sxd_pull_parser;
pub use sxd_validation;

use sxd_pull_parser::Parser;
use sxd_validation::Validator;

pub type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

pub fn parse(data: &[u8]) -> Result<usize> {
    let mut parser = Validator::new(Parser::new(data));
    let mut count = 0;

    while let Some(v) = parser.next_str() {
        v?;
        count += 1;
    }

    Ok(count)
}

pub fn assert_both(data: &[u8]) {
    let data = trim_to_first_nul(data);

    match (libxml2_sys::parse(data), parse(data)) {
        (Ok(_), Ok(_)) => {}
        (Err(_), Err(_)) => {}

        (Ok(lx), Err(e)) => {
            panic!("libxml2 parsed {data:?} as {lx}, but we failed due to {e} / {e:?}");
        }

        (Err(e), Ok(_)) => {
            panic!("libxml2 failed to parse {data:?} due to {e}, but we succeeded");
        }
    }
}

pub fn assert_both_parse(data: &[u8]) {
    let data = trim_to_first_nul(data);

    let lx = match libxml2_sys::parse(data) {
        Ok(lx) => lx,
        Err(_) => return,
    };

    if let Err(e) = parse(data) {
        panic!("libxml2 parsed {data:?} as {lx}, but we failed due to {e} / {e:?}");
    }
}

pub fn assert_both_fail(data: &[u8]) {
    let data = trim_to_first_nul(data);

    let lx = match libxml2_sys::parse(data) {
        Ok(_) => return,
        Err(e) => e,
    };

    if parse(data).is_ok() {
        panic!("libxml2 failed to parse {data:?} due to {lx:?}, but we succeeded");
    }
}

fn trim_to_first_nul(data: &[u8]) -> &[u8] {
    let first_nul = memchr::memchr(0, data).unwrap_or(data.len());
    &data[..first_nul]
}
