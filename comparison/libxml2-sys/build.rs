use std::{env, path::PathBuf};

fn main() {
    let lib = pkg_config::probe_library("libxml-2.0").expect("Can't find libxml2");

    let mut builder = bindgen::Builder::default();

    for inc in &lib.include_paths {
        // The input header we would like to generate
        // bindings for.
        let p = inc.join("libxml/parser.h");
        builder = builder
            .header(p.display().to_string())
            .clang_arg(format!("-I{}", inc.display()));
    }

    let builder = builder
        // Uses 128-bit types which aren't FFI safe
        .blocklist_function("qecvt")
        .blocklist_function("qecvt_r")
        .blocklist_function("qfcvt")
        .blocklist_function("qfcvt_r")
        .blocklist_function("qgcvt")
        .blocklist_function("strtold")
        .blocklist_type("_Float64x")
        .blocklist_type("max_align_t")
        // Tell cargo to invalidate the built crate whenever any of the
        // included header files changed.
        .parse_callbacks(Box::new(bindgen::CargoCallbacks));

    dbg!(&builder);

    let bindings = builder
        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    // Write the bindings to the $OUT_DIR/bindings.rs file.
    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
