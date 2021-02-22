#[macro_use]
extern crate afl;

use comparison::libxml2_sys;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            if s.contains('\u{0}') {
                return;
            }

            if let Ok(_) = libxml2_sys::parse(&s) {
                return;
            }

            if let Ok(_) = comparison::parse(&s) {
                panic!("libxml2 failed to parse {:?}, but we didn't", s);
            }
        }
    });
}
