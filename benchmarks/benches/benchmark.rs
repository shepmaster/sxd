use benchmarks::{generate_with_duplicates, WITHOUT_DUPLICATES, WITH_DUPLICATES};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use hashbrown::HashSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use std::{convert::TryInto, iter};
use sxd_string_slab::UnsafeArena;

fn without_duplicates(c: &mut Criterion) {
    Lazy::force(&WITHOUT_DUPLICATES);
    Lazy::force(&WITH_DUPLICATES);

    let mut group = c.benchmark_group("without-duplicates");
    group.throughput(Throughput::Elements(
        WITHOUT_DUPLICATES.len().try_into().unwrap(),
    ));

    group.bench_function("sxd_string_slab::UnsafeArena", |b| {
        b.iter(|| {
            let mut arena = UnsafeArena::new();
            for s in &*WITHOUT_DUPLICATES {
                arena.intern(s);
            }
        })
    });

    group.bench_function("hashbrown::HashSet", |b| {
        b.iter(|| {
            let mut arena = HashSet::new();
            for s in &*WITHOUT_DUPLICATES {
                arena.get_or_insert_owned(s);
            }
        })
    });

    group.finish();
}

fn with_duplicates(c: &mut Criterion) {
    Lazy::force(&WITHOUT_DUPLICATES);
    Lazy::force(&WITH_DUPLICATES);

    let mut group = c.benchmark_group("with-duplicates");
    group.throughput(Throughput::Elements(
        WITH_DUPLICATES.len().try_into().unwrap(),
    ));

    group.bench_function("sxd_string_slab::UnsafeArena", |b| {
        b.iter(|| {
            let mut arena = UnsafeArena::new();
            for s in &*WITH_DUPLICATES {
                arena.intern(s);
            }
        })
    });

    group.bench_function("hashbrown::HashSet", |b| {
        b.iter(|| {
            let mut arena = HashSet::new();
            for s in &*WITH_DUPLICATES {
                arena.get_or_insert_owned(s);
            }
        })
    });

    group.finish();
}

fn slab_vs_string_length(c: &mut Criterion) {
    Lazy::force(&WITHOUT_DUPLICATES);
    Lazy::force(&WITH_DUPLICATES);

    let double_in_range = |l: usize, u: usize| {
        iter::successors(Some(l), |v| v.checked_mul(2)).take_while(move |&v| v <= u)
    };

    let str_max_lens = double_in_range(8, 2 * 1024);
    let slab_sizes = double_in_range(1024, 256 * 1024);
    let space = slab_sizes.cartesian_product(str_max_lens);

    let mut group = c.benchmark_group("slab-vs-string-length");

    for args in space {
        let (slab_size, max_str_len) = args;
        let id = BenchmarkId::from_parameter(format_args!("{:06}x{:04}", slab_size, max_str_len));
        let input = generate_with_duplicates(10_000, max_str_len);

        group.bench_with_input(id, &args, |b, &(slab_size, _max_str_len)| {
            b.iter(|| {
                let mut arena = UnsafeArena::with_slab_size(slab_size);
                for s in &input {
                    arena.intern(s);
                }
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    without_duplicates,
    with_duplicates,
    slab_vs_string_length,
);
criterion_main!(benches);
