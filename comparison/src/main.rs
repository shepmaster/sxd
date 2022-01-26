use std::{env, fs};

fn main() {
    let mode = env::args().nth(1).expect("Mode missing");
    let filename = env::args().nth(2).expect("Filename missing");
    let data = fs::read(&filename).expect("Could not read data from file");

    match &*mode {
        "fail" => {
            eprintln!("assert_both_fail...");
            comparison::assert_both_fail(&data);
        }
        "parse" => {
            eprintln!("assert_both_parse...");
            comparison::assert_both_parse(&data);
        }
        _ => panic!("Unknown mode (fail / parse)"),
    }
}
