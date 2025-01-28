#![deny(rust_2018_idioms)]

#[macro_use]
extern crate afl;

fn main() {
    fuzz!(|d: &[u8]| {
        comparison::assert_both_fail(d);
    });
}
