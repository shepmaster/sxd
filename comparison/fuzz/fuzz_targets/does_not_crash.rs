#![deny(rust_2018_idioms)]
#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|s: String| {
    let _ = comparison::parse(&s);
});
