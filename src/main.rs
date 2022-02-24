#![deny(rust_2018_idioms)]

use argh::FromArgs;
use formatter::Formatter;
use pull_parser::Parser;
use std::{
    env,
    fs::File,
    io::{self, BufWriter, Read, Write},
    str::FromStr,
};
use token::Source;
use validation::Validator;

type Error = Box<dyn std::error::Error>;
type Result<T = (), E = Error> = std::result::Result<T, E>;

/// The developer utility for SXD
#[derive(Debug, FromArgs)]
struct Args {
    /// do not output the parsed XML
    #[argh(switch, short = 'q')]
    quiet: bool,

    /// how many bytes to buffer when reading and writing
    #[argh(option)]
    buffer_size: Option<usize>,

    /// how many bytes to buffer when reading.
    ///
    /// Defaults to `buffer-size`.
    #[argh(option)]
    input_buffer_size: Option<usize>,

    /// how many bytes to buffer when writing.
    ///
    /// Defaults to `buffer-size`.
    #[argh(option)]
    output_buffer_size: Option<usize>,

    /// the file to read
    #[argh(positional)]
    filename: String,
}

impl Args {
    const DEFAULT_BUFFER_SIZE: usize = 16 * 1024 * 1024;

    fn apply_environment_variables(&mut self) {
        self.quiet = self.quiet || env::var_os("QUIET").is_some();

        self.buffer_size.ambient_value("BUFFER_SIZE");
        self.input_buffer_size.ambient_value("INPUT_BUFFER_SIZE");
        self.output_buffer_size.ambient_value("OUTPUT_BUFFER_SIZE");
    }

    fn into_options(self) -> Options {
        let Self {
            quiet,
            buffer_size,
            input_buffer_size,
            output_buffer_size,
            filename,
        } = self;

        let buffer_size = buffer_size.unwrap_or(Self::DEFAULT_BUFFER_SIZE);
        let input_buffer_size = input_buffer_size.unwrap_or(buffer_size);
        let output_buffer_size = output_buffer_size.unwrap_or(buffer_size);

        Options {
            quiet,
            input_buffer_size,
            output_buffer_size,
            filename,
        }
    }
}

#[derive(Debug)]
struct Options {
    quiet: bool,
    input_buffer_size: usize,
    output_buffer_size: usize,
    filename: String,
}

impl Options {
    fn from_env_and_command_line() -> Self {
        let mut args: Args = argh::from_env();
        args.apply_environment_variables();
        args.into_options()
    }
}

trait AmbientValue {
    fn ambient_value(&mut self, env_var_name: &str);
}

impl<T> AmbientValue for Option<T>
where
    T: FromStr,
{
    fn ambient_value(&mut self, env_var_name: &str) {
        if self.is_none() {
            if let Ok(v) = env::var(env_var_name) {
                *self = v.parse().ok();
            }
        }
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let Options {
        quiet,
        input_buffer_size,
        output_buffer_size,
        filename,
    } = Options::from_env_and_command_line();

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
