use benchmarks::{DUPLICATES, DUPLICATES_STRING, NO_DUPLICATES, NO_DUPLICATES_STRING};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use hashbrown::HashSet;
use once_cell::sync::Lazy;
use std::convert::TryInto;
use sxd_string_slab::StringArena;

fn criterion_benchmark(c: &mut Criterion) {
    Lazy::force(&NO_DUPLICATES_STRING);
    Lazy::force(&DUPLICATES_STRING);

    let slab_sizes: Vec<_> = (0..=8).map(|power| 1024 * (1 << power)).collect();

    let mut group = c.benchmark_group("duplicates/no");
    group.throughput(Throughput::Elements(
        NO_DUPLICATES.len().try_into().unwrap(),
    ));

    for slab_size in &slab_sizes {
        let id = BenchmarkId::new("sxd_string_slab::StringArena", slab_size);

        group.bench_with_input(id, slab_size, |b, &slab_size| {
            b.iter(|| {
                let mut arena = StringArena::with_slab_size(slab_size);
                for s in NO_DUPLICATES_STRING.lines() {
                    arena.intern(s);
                }
            })
        });
    }

    group.bench_function("hashbrown::HashSet", |b| {
        b.iter(|| {
            let mut arena = HashSet::new();
            for s in NO_DUPLICATES_STRING.lines() {
                arena.get_or_insert_owned(s);
            }
        })
    });
    group.finish();

    let mut group = c.benchmark_group("duplicates/yes");
    group.throughput(Throughput::Elements(DUPLICATES.len().try_into().unwrap()));

    for slab_size in &slab_sizes {
        let id = BenchmarkId::new("sxd_string_slab::StringArena", slab_size);

        group.bench_with_input(id, slab_size, |b, &slab_size| {
            b.iter(|| {
                let mut arena = StringArena::with_slab_size(slab_size);
                for s in DUPLICATES_STRING.lines() {
                    arena.intern(s);
                }
            })
        });
    }

    group.bench_function("hashbrown::HashSet", |b| {
        b.iter(|| {
            let mut arena = HashSet::new();
            for s in DUPLICATES_STRING.lines() {
                arena.get_or_insert_owned(s);
            }
        })
    });

    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
