#![no_main]

use comparison::libxml2_sys;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: String| {
    // libxml2 doesn't handle embedded NULs
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
});
