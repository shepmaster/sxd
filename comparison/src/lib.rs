pub use libxml2_sys;
pub use sxd_pull_parser;

use sxd_pull_parser::{Parser, ReadAdapter, TokenSource};

pub type Result<T = (), E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

pub fn parse(s: &str) -> Result<usize> {
    let src = ReadAdapter::new(s.as_bytes());
    let mut parser = Parser::new(src);
    let mut count = 0;

    futures::executor::block_on(async {
        loop {
            match parser.next().await {
                Some(v) => {
                    v?;
                    count += 1;
                    continue;
                }
                None => return Ok(count),
            }
        }
    })
}
