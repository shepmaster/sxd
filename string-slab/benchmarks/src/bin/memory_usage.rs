use benchmarks::{
    alloc::TrackingAllocator, env_or, WITHOUT_DUPLICATES_STRING, WITH_DUPLICATES_STRING,
};
use hashbrown::HashSet;
use std::{convert::TryFrom, sync::LazyLock};
use sxd_string_slab::UnsafeArena;

#[global_allocator]
static A: TrackingAllocator = TrackingAllocator;

fn main() {
    let show_map = env_or("SHOW_MAP", false);
    LazyLock::force(&WITHOUT_DUPLICATES_STRING);
    LazyLock::force(&WITH_DUPLICATES_STRING);

    let total_length: usize = WITH_DUPLICATES_STRING.lines().map(str::len).sum();
    let total_length_f64 = f64::from(u32::try_from(total_length).unwrap());

    eprintln!("String data of {} bytes", total_length);

    let (_arena, alloc_size, alloc_count, alloc_map) = TrackingAllocator::track_allocations(|| {
        let mut arena = UnsafeArena::new();
        for s in WITH_DUPLICATES_STRING.lines() {
            arena.intern(s);
        }
        arena
    });

    let percent = f64::from(u32::try_from(alloc_size).unwrap()) / total_length_f64 * 100.0;
    eprintln!(
        "sxd_string_slab::UnsafeArena: {} bytes ({:.2}%) in {} allocations",
        alloc_size, percent, alloc_count
    );
    if show_map {
        eprintln!("{:?}", alloc_map);
    }

    let (_arena, alloc_size, alloc_count, alloc_map) = TrackingAllocator::track_allocations(|| {
        let mut arena = HashSet::new();
        for s in WITH_DUPLICATES_STRING.lines() {
            arena.get_or_insert_owned(s);
        }
        arena
    });

    let percent = f64::from(u32::try_from(alloc_size).unwrap()) / total_length_f64 * 100.0;
    eprintln!(
        "hashbrown::HashSet: {} bytes ({:.2}%) in {} allocations",
        alloc_size, percent, alloc_count
    );
    if show_map {
        eprintln!("{:?}", alloc_map);
    }
}
