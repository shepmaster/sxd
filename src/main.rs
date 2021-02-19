use formatter::Formatter;
use pull_parser::Parser;
use std::{
    env,
    fs::File,
    io::{self, BufWriter, Read, Write},
    str::FromStr,
};

type Error = Box<dyn std::error::Error>;
type Result<T = (), E = Error> = std::result::Result<T, E>;

fn env_or<T>(name: &str, def: T) -> T
where
    T: FromStr,
{
    env::var(name)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(def)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let quiet = env::var_os("QUIET").is_some();
    let buffer_size = env_or("BUFFER_SIZE", 16 * 1024 * 1024);
    let input_buffer_size = env_or("INPUT_BUFFER_SIZE", buffer_size);
    let output_buffer_size = env_or("OUTPUT_BUFFER_SIZE", buffer_size);

    let filename = env::args().skip(1).next().expect("Needs one argument");
    let file = File::open(&filename).expect("Unable to open file");

    let parser = Parser::with_buffer_capacity(file, input_buffer_size);

    let count = if quiet {
        let out = io::sink();
        let out = BufWriter::with_capacity(output_buffer_size, out);
        stream_output(parser, out)
    } else {
        let out = io::stdout();
        let out = out.lock();
        let out = BufWriter::with_capacity(output_buffer_size, out);
        stream_output(parser, out)
    }?;

    eprintln!("Parsed {} tokens", count);

    Ok(())
}

fn stream_output(mut parser: Parser<impl Read>, out: impl Write) -> Result<usize> {
    let mut count = 0;
    let mut fmt = Formatter::new(out);

    while let Some(token) = parser.next() {
        let token = token?;
        count += 1;
        fmt.write_token(token)?;
    }

    Ok(count)
}
