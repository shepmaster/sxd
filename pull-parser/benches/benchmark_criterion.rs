use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use sxd_pull_parser::Parser;
use token::Source;

fn filename() -> &'static str {
    option_env!("FILE").unwrap_or("examples/standard.xml")
}

fn parse(file: &str) -> usize {
    let mut parser = Parser::with_buffer_capacity(file.as_bytes(), 16 * 1024 * 1024);

    let mut count = 0usize;
    while let Some(v) = parser.next_index() {
        v.unwrap();
        count += 1;
    }

    count
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("sxd-pull-parser");
    group.sample_size(20);

    let file = std::fs::read_to_string(filename()).unwrap();

    group.bench_with_input(
        BenchmarkId::new("raw parse speed", filename()),
        &file,
        |b, file| {
            b.iter(|| parse(file));
        },
    );

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
