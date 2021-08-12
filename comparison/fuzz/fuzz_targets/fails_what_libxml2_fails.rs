#![no_main]

use comparison::libxml2_sys;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: String| {
    if libxml2_sys::parse(&s).is_ok() {
        return;
    };

    if comparison::parse(&s).is_ok() {
        panic!("libxml2 failed to parse {:?}, but we didn't", s);
    }
});
