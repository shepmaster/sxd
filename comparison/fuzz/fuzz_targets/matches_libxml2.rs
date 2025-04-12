#![deny(rust_2018_idioms)]
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|d: &[u8]| {
    comparison::assert_both(d);
});
