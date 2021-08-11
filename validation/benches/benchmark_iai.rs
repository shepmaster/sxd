use pull_parser::Parser;
use sxd_validation::Validator;

fn filename() -> &'static str {
    option_env!("FILE").unwrap_or("examples/standard.xml")
}

fn parse(file: &str) -> usize {
    let parser = Parser::with_buffer_capacity(file.as_bytes(), 16 * 1024 * 1024);
    let mut parser = Validator::new(parser);

    let mut count = 0usize;
    while let Some(v) = parser.next_index() {
        v.unwrap();
        count += 1;
    }

    count
}

fn benchmark() {
    let file = std::fs::read_to_string(filename()).unwrap();

    let count = parse(&file);

    eprintln!("Parsed {} tokens", count);
}

iai::main!(benchmark);
