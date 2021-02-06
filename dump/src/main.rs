use std::{
    env,
    fs::File,
    io::{self, BufWriter, Read, Write},
};
use sxd_pull_parser::blocking::{Parser, TokenSource};

type Error = Box<dyn std::error::Error>;
type Result<T = (), E = Error> = std::result::Result<T, E>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let quiet = env::var_os("QUIET").is_some();
    let filename = env::args().skip(1).next().expect("Needs one argument");
    let file = File::open(&filename).expect("Unable to open file");

    let parser = Parser::with_buffer_capacity(file, 16 * 1024 * 1024);

    let count = if quiet {
        let out = io::sink();
        let out = BufWriter::with_capacity(16 * 1024 * 1024, out);
        stream_output(parser, out)
    } else {
        let out = io::stdout();
        let out = out.lock();
        let out = BufWriter::with_capacity(16 * 1024 * 1024, out);
        stream_output(parser, out)
    }?;

    eprintln!("Parsed {} tokens", count);

    Ok(())
}

fn stream_output(mut parser: Parser<impl Read>, mut out: impl Write) -> Result<usize> {
    let mut count = 0;
    while let Some(v) = parser.next() {
        let v = v?;

        count += 1;

        writeln!(out, "{:?}", v)?;
    }
    Ok(count)
}
