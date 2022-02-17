#![deny(rust_2018_idioms)]

use argh::FromArgs;
use formatter::Formatter;
use pull_parser::{Fuse as Fuser, Parser};
use std::{
    env,
    fs::File,
    io::{self, BufWriter, Write},
    str::FromStr,
};
use token::Source;
use validation::Validator;

type Error = Box<dyn std::error::Error>;
type Result<T = (), E = Error> = std::result::Result<T, E>;

/// The developer utility for SXD
#[derive(Debug, FromArgs)]
struct Args {
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

    /// how to process the XML file.
    ///
    /// Valid choices: parse, fuse, validate.
    #[argh(option)]
    process: Option<ProcessKind>,

    /// how to output the XML file.
    ///
    /// Valid choices: count, null, stdout.
    #[argh(option)]
    output: Option<OutputKind>,

    /// the file to read
    #[argh(positional)]
    filename: String,
}

impl Args {
    const DEFAULT_BUFFER_SIZE: usize = 16 * 1024 * 1024;

    fn apply_environment_variables(&mut self) {
        self.buffer_size.ambient_value("BUFFER_SIZE");
        self.input_buffer_size.ambient_value("INPUT_BUFFER_SIZE");
        self.output_buffer_size.ambient_value("OUTPUT_BUFFER_SIZE");

        self.process.ambient_value("PROCESS");
        self.output.ambient_value("OUTPUT");
    }

    fn into_options(self) -> Options {
        let Self {
            buffer_size,
            input_buffer_size,
            output_buffer_size,
            process,
            output,
            filename,
        } = self;

        let buffer_size = buffer_size.unwrap_or(Self::DEFAULT_BUFFER_SIZE);
        let input_buffer_size = input_buffer_size.unwrap_or(buffer_size);
        let output_buffer_size = output_buffer_size.unwrap_or(buffer_size);

        let process = process.unwrap_or_default();
        let output = output.unwrap_or_default();

        Options {
            input_buffer_size,
            output_buffer_size,
            process,
            output,
            filename,
        }
    }
}

#[derive(Debug)]
struct Options {
    input_buffer_size: usize,
    output_buffer_size: usize,
    process: ProcessKind,
    output: OutputKind,
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
        input_buffer_size,
        output_buffer_size,
        process,
        output,
        filename,
    } = Options::from_env_and_command_line();

    let gen_parser = || {
        let file = File::open(&filename).expect("Unable to open file");
        Parser::with_buffer_capacity(file, input_buffer_size)
    };

    let gen_fuser = || Fuser::new(gen_parser());

    let gen_validator = || Validator::new(gen_parser());

    let gen_quiet = || {
        let out = io::sink();
        BufWriter::with_capacity(output_buffer_size, out)
    };

    let gen_stdout = || {
        let out = io::stdout();
        let out = out.lock();
        BufWriter::with_capacity(output_buffer_size, out)
    };

    use {OutputKind as OK, ProcessKind as PK};
    let mut process: Box<dyn Process> = match (process, output) {
        (PK::Parse, OK::Count) => CountIndex::new_object(gen_parser()),
        (PK::Parse, OK::Null) => Format::new_object(gen_parser(), gen_quiet()),
        (PK::Parse, OK::Stdout) => Format::new_object(gen_parser(), gen_stdout()),
        (PK::Fuse, OK::Count) => CountIndex::new_object(gen_fuser()),
        (PK::Fuse, OK::Null) => Format::new_object(gen_fuser(), gen_quiet()),
        (PK::Fuse, OK::Stdout) => Format::new_object(gen_fuser(), gen_stdout()),
        (PK::Validate, OK::Count) => CountIndex::new_object(gen_validator()),
        (PK::Validate, OK::Null) => Format::new_object(gen_validator(), gen_quiet()),
        (PK::Validate, OK::Stdout) => Format::new_object(gen_validator(), gen_stdout()),
    };

    let count = process.process()?;

    eprintln!("Parsed {} tokens", count);

    Ok(())
}

trait Process {
    fn process(&mut self) -> Result<usize>;
}

struct CountIndex<I>(I);

impl<'a, I> CountIndex<I>
where
    I: Source + 'a,
{
    fn new_object(i: I) -> Box<dyn Process + 'a> {
        Box::new(Self(i))
    }
}

impl<I: Source> Process for CountIndex<I> {
    fn process(&mut self) -> Result<usize> {
        let mut count = 0;

        while let Some(token) = self.0.next_index() {
            let _token = token?;
            count += 1;
        }

        Ok(count)
    }
}

struct Format<I, W>(I, W);

impl<'a, I, W> Format<I, W>
where
    I: Source + 'a,
    for<'b> I::StrKind<'b>: formatter::FormatKind,
    W: Write + 'a,
{
    fn new_object(i: I, w: W) -> Box<dyn Process + 'a> {
        Box::new(Self(i, w))
    }
}

impl<I, W> Process for Format<I, W>
where
    I: Source,
    for<'a> I::StrKind<'a>: formatter::FormatKind,
    W: Write,
{
    fn process(&mut self) -> Result<usize> {
        let mut count = 0;
        let mut fmt = Formatter::new(&mut self.1);

        while let Some(token) = self.0.next_str() {
            let token = token?;
            count += 1;

            fmt.write_token(token)?;
        }

        Ok(count)
    }
}

#[derive(Debug)]
enum ProcessKind {
    Parse,
    Fuse,
    Validate,
}

impl Default for ProcessKind {
    fn default() -> Self {
        ProcessKind::Validate
    }
}

impl FromStr for ProcessKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "parse" => Ok(ProcessKind::Parse),
            "fuse" => Ok(ProcessKind::Fuse),
            "validate" => Ok(ProcessKind::Validate),
            _ => Err(format!("unknown process type {s}")),
        }
    }
}

#[derive(Debug)]
enum OutputKind {
    Count,
    Null,
    Stdout,
}

impl Default for OutputKind {
    fn default() -> Self {
        OutputKind::Stdout
    }
}

impl FromStr for OutputKind {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "count" => Ok(OutputKind::Count),
            "null" => Ok(OutputKind::Null),
            "stdout" => Ok(OutputKind::Stdout),
            _ => Err(format!("unknown output type {s}")),
        }
    }
}
