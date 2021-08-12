#[macro_use]
extern crate afl;

use comparison::libxml2_sys;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            if libxml2_sys::parse(s).is_ok() {
                return;
            }

            if comparison::parse(s).is_ok() {
                panic!("libxml2 failed to parse {:?}, but we didn't", s);
            }
        }
    });
}
