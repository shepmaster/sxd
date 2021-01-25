use benchmarks::{DUPLICATES, DUPLICATES_STRING, NO_DUPLICATES, NO_DUPLICATES_STRING};
use criterion::{criterion_group, criterion_main, Criterion, Throughput};
use hashbrown::HashSet;
use once_cell::sync::Lazy;
use std::convert::TryInto;
use sxd_string_slab::StringArena;

fn criterion_benchmark(c: &mut Criterion) {
    Lazy::force(&NO_DUPLICATES_STRING);
    Lazy::force(&DUPLICATES_STRING);

    {
        let mut group = c.benchmark_group("no duplicates");
        group.throughput(Throughput::Elements(
            NO_DUPLICATES.len().try_into().unwrap(),
        ));

        group.bench_function("sxd_string_slab::StringArena", |b| {
            b.iter(|| {
                let mut arena = StringArena::new();
                for s in NO_DUPLICATES_STRING.lines() {
                    arena.intern(s);
                }
            })
        });

        group.bench_function("hashbrown::HashSet", |b| {
            b.iter(|| {
                let mut arena = HashSet::new();
                for s in NO_DUPLICATES_STRING.lines() {
                    arena.get_or_insert_owned(s);
                }
            })
        });
    }

    {
        let mut group = c.benchmark_group("duplicates");
        group.throughput(Throughput::Elements(DUPLICATES.len().try_into().unwrap()));

        group.bench_function("sxd_string_slab::StringArena", |b| {
            b.iter(|| {
                let mut arena = StringArena::new();
                for s in DUPLICATES_STRING.lines() {
                    arena.intern(s);
                }
            })
        });

        group.bench_function("hashbrown::HashSet", |b| {
            b.iter(|| {
                let mut arena = HashSet::new();
                for s in DUPLICATES_STRING.lines() {
                    arena.get_or_insert_owned(s);
                }
            })
        });
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
