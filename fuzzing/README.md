

# Benchmarks

```
FILE=../standard.xml cargo bench -p sxd-pull-parser --bench benchmark_iai
```

```
hyperfine --setup 'cargo build --release' --warmup 3 'QUIET=1 ./target/release/sxd path/to/standard.xml'
```

# libFuzzer

```
cargo fuzz run lf_matches_libxml2
```

You can choose different binaries to fuzz — change the last argument
appropriately.


# AFL

https://rust-fuzz.github.io/book/afl/tutorial.html

Enter the directory:

```
cd using-afl
```

Setup input and output directories:

```
mkdir -p input output
```

Add a seed XML file:

```
cat <<EOF > input/seed.xml
<?xml version="1.0"?>
<a name="a">
  <b1 name="b1" xmlns:ns1="1" xmlns:ns2="2" />
  <!--hello-->
  <b3 name="b3" />
  <?hello world?>
</a>
EOF
```

Build the binaries, then run them:

```
cargo afl build
cargo afl fuzz -x xml.dict -i input -o output ../../target/debug/afl_fails_what_libxml2_fails
```

You can choose different binaries to fuzz — change the last argument
appropriately.

# Proptest

Enter the directory:

```
cd using-proptest
```

Run the test:

```
PROPTEST_CASES=100000 cargo test
```

# Troubleshooting

## On macOS

May need to inform the build about the Homebrew installation of libxml2:

```
export PKG_CONFIG_PATH="$(brew --prefix)/opt/libxml2/lib/pkgconfig"
```

## On WSL2

May need to specify the `llvm-config` binary name

```
LLVM_CONFIG=llvm-config-10
```
