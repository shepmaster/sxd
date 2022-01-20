#![deny(rust_2018_idioms)]

use hashbrown::HashSet;
use itertools::Itertools;
use once_cell::sync::Lazy;
use rand::{distributions::Alphanumeric, rngs::StdRng, seq::SliceRandom, Rng, SeedableRng};
use std::{env, iter, str::FromStr};

pub mod alloc {
    use backtrace::Backtrace;
    use hashbrown::HashMap;
    use once_cell::sync::Lazy;
    use std::{
        alloc::{GlobalAlloc, Layout, System},
        hash::{Hash, Hasher},
        mem,
        sync::{
            atomic::{AtomicBool, AtomicUsize, Ordering},
            Mutex,
        },
    };

    pub struct TrackingAllocator;

    static RECURSIVE: AtomicBool = AtomicBool::new(false);
    static MEMORY_IN_USE: AtomicUsize = AtomicUsize::new(0);
    static N_ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);
    static ALLOCATION_MAP: Lazy<Mutex<AllocMap>> = Lazy::new(Default::default);

    #[derive(Debug, Default)]
    pub struct AllocMap(HashMap<Trace, Vec<usize>>);

    #[derive(Debug)]
    struct Trace(Backtrace);

    impl Trace {
        fn new() -> Self {
            Self(Backtrace::new_unresolved())
        }
    }

    impl Hash for Trace {
        fn hash<H>(&self, h: &mut H)
        where
            H: Hasher,
        {
            for f in self.0.frames() {
                f.ip().hash(h);
            }
        }
    }

    impl PartialEq for Trace {
        fn eq(&self, other: &Self) -> bool {
            self.0
                .frames()
                .iter()
                .map(|f| f.ip())
                .eq(other.0.frames().iter().map(|f| f.ip()))
        }
    }

    unsafe impl GlobalAlloc for TrackingAllocator {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            let inside_alloc = RECURSIVE.fetch_or(true, Ordering::SeqCst);

            if !inside_alloc {
                MEMORY_IN_USE.fetch_add(layout.size(), Ordering::SeqCst);
                N_ALLOCATIONS.fetch_add(1, Ordering::SeqCst);
                ALLOCATION_MAP
                    .lock()
                    .expect("Mutex Poisoned")
                    .0
                    .entry(Trace::new())
                    .or_insert_with(Vec::new)
                    .push(layout.size());
            }

            RECURSIVE.store(inside_alloc, Ordering::SeqCst);

            System.alloc(layout)
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            let inside_alloc = RECURSIVE.fetch_or(true, Ordering::SeqCst);

            if !inside_alloc {
                MEMORY_IN_USE.fetch_sub(layout.size(), Ordering::SeqCst);
            }
            RECURSIVE.store(inside_alloc, Ordering::SeqCst);

            System.dealloc(ptr, layout)
        }
    }

    impl Eq for Trace {}

    impl TrackingAllocator {
        pub fn track_allocations<R>(f: impl FnOnce() -> R) -> (R, usize, usize, AllocMap) {
            let start_size = MEMORY_IN_USE.load(Ordering::SeqCst);
            let start_allocations = N_ALLOCATIONS.load(Ordering::SeqCst);
            let start_map = mem::take(&mut *ALLOCATION_MAP.lock().expect("Mutex Poisoned"));

            let r = f();

            let end_size = MEMORY_IN_USE.load(Ordering::SeqCst);
            let end_allocations = N_ALLOCATIONS.load(Ordering::SeqCst);
            let end_map = mem::replace(
                &mut *ALLOCATION_MAP.lock().expect("Mutex Poisoned"),
                start_map,
            );

            let end_map = AllocMap(
                end_map
                    .0
                    .into_iter()
                    .map(|(mut k, v)| {
                        k.0.resolve();
                        (k, v)
                    })
                    .collect(),
            );

            (
                r,
                end_size - start_size,
                end_allocations - start_allocations,
                end_map,
            )
        }
    }
}

pub fn env_or<T>(name: &str, default: T) -> T
where
    T: FromStr,
{
    env_or_else(name, || default)
}

pub fn env_or_else<T>(name: &str, default: impl FnOnce() -> T) -> T
where
    T: FromStr,
{
    env::var(name)
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or_else(default)
}

pub fn rng() -> StdRng {
    static SEED: Lazy<u64> = Lazy::new(|| {
        let seed = env_or_else("BENCHMARK_SEED", || rand::thread_rng().gen());
        eprintln!("Using random seed {} (can be set via BENCHMARK_SEED)", seed);
        seed
    });

    StdRng::seed_from_u64(*SEED)
}

fn string_iter(rng: &mut impl Rng, max_len: usize) -> impl Iterator<Item = String> + '_ {
    iter::from_fn(move || {
        let string_len = rng.gen_range(0..max_len);
        let s = rng
            .sample_iter(Alphanumeric)
            .take(string_len)
            .map(char::from)
            .collect();
        Some(s)
    })
}

pub fn generate_without_duplicates(n_items: usize, max_str_len: usize) -> HashSet<String> {
    let rng = &mut rng();

    string_iter(rng, max_str_len).take(n_items).collect()
}

pub fn generate_with_duplicates(n_items: usize, max_str_len: usize) -> Vec<String> {
    let rng = &mut rng();

    let no_dupes: HashSet<_> = string_iter(rng, max_str_len).take(n_items).collect();
    let mut no_dupes: Vec<_> = no_dupes.into_iter().collect();

    let n_dupes = rng.gen_range(0..no_dupes.len());
    let dupes: Vec<_> = no_dupes.choose_multiple(rng, n_dupes).cloned().collect();
    no_dupes.extend(dupes);
    no_dupes.shuffle(rng);

    no_dupes
}

pub static WITHOUT_DUPLICATES: Lazy<HashSet<String>> = Lazy::new(|| {
    let n_items = env_or("N_WITHOUT_DUPLICATES", 10_000);
    generate_without_duplicates(n_items, 2048)
});

pub static WITHOUT_DUPLICATES_STRING: Lazy<String> =
    Lazy::new(|| WITHOUT_DUPLICATES.iter().join("\n"));

pub static WITH_DUPLICATES: Lazy<Vec<String>> = Lazy::new(|| {
    let n_items = env_or("N_WITH_DUPLICATES", 10_000);
    generate_with_duplicates(n_items, 2048)
});

pub static WITH_DUPLICATES_STRING: Lazy<String> = Lazy::new(|| WITH_DUPLICATES.iter().join("\n"));
