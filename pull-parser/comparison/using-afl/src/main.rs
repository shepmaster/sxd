#[macro_use]
extern crate afl;

use comparison::libxml2_sys;

fn main() {
    fuzz!(|data: &[u8]| {
        if let Ok(s) = std::str::from_utf8(data) {
            if s.contains('\u{0}') {
                return;
            }

            let lx = match libxml2_sys::parse(&s) {
                Ok(lx) => lx,
                Err(_) => return,
            };

            if let Err(e) = comparison::parse(&s) {
                panic!("libxml2 parsed {:?} as {}, but we couldn't ({})", s, lx, e,);
            }
        }
    });
}
