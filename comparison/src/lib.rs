pub use libxml2_sys;
pub use sxd_pull_parser;

use sxd_pull_parser::Parser;

pub type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

pub fn parse(s: &str) -> Result<usize> {
    let mut parser = Parser::new(s.as_bytes());
    let mut count = 0;

    while let Some(v) = parser.next() {
        v?;
        count += 1;
    }

    Ok(count)
}
