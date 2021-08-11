use formatter::Formatter;
use pull_parser::Parser;
use std::{
    env,
    fs::File,
    io::{self, BufWriter, Read, Write},
    str::FromStr,
};
use validation::Validator;

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

    let filename = env::args().nth(1).expect("Needs one argument");
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

fn stream_output(parser: Parser<impl Read>, out: impl Write) -> Result<usize> {
    let mut count = 0;
    let mut validator = Validator::new(parser);
    let mut fmt = Formatter::new(out);

    while let Some(token) = validator.next_str() {
        let token = token?;
        count += 1;

        fmt.write_token(token)?;
    }

    Ok(count)
}
