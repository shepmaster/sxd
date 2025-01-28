use criterion::{Criterion, black_box, criterion_group, criterion_main};
use std::{env, fs::File, io::Read as _};
use sxd_xml_str::SliceExt as Ours;
use sxd_xml_str_comparison::SliceExt as Naive;

pub fn random_bytes(c: &mut Criterion) {
    let data = random_data();

    let mut g = c.benchmark_group("random_bytes");
    g.bench_function("ours", |b| b.iter(|| many(&data, Ours::nc_name)));
    g.bench_function("naive", |b| b.iter(|| many(&data, Naive::nc_name)));
}

pub fn xml_document(c: &mut Criterion) {
    let data = xml_data();

    let mut g = c.benchmark_group("xml_document");
    g.bench_function("ours", |b| b.iter(|| many(&data, Ours::nc_name)));
    g.bench_function("naive", |b| b.iter(|| many(&data, Naive::nc_name)));
}

fn many(d: &[u8], f: impl Fn(&[u8]) -> (&str, &[u8])) {
    // Looking at just 32 bytes is a bit of a cop-out, but the naive
    // implementation can validate the entire string, even if the
    // first byte isn't a valid name.
    for d in d.windows(32) {
        black_box(f(d));
    }
}

fn random_data() -> Vec<u8> {
    use rand::distributions::{Distribution, Standard};
    Standard
        .sample_iter(&mut rand::thread_rng())
        .take(1024 * 1024)
        .collect()
}

fn xml_data() -> Vec<u8> {
    const VAR_NAME: &str = "COMPARISON_XML_FILE_NAME";
    let fname = match env::var(VAR_NAME) {
        Ok(v) => v,
        Err(_) => panic!("Set the {VAR_NAME} environment variable to the path of a large XML file"),
    };

    let mut data = Vec::new();
    let f = File::open(fname).unwrap();
    f.take(1024 * 1024).read_to_end(&mut data).unwrap();
    data
}

criterion_group!(benches, random_bytes, xml_document);
criterion_main!(benches);
