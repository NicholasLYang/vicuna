use std::env::current_dir;
use std::path::PathBuf;

fn get_repo_root() -> PathBuf {
    let mut dir = current_dir().unwrap();
    dir.push(".."); // crates
    dir.push(".."); // repository root
    dir
}

fn main() {
    println!("cargo:rerun-if-changed=../../tree-sitter-vicuna/grammar.js");
    let mut dir = get_repo_root();
    dir.push("tree-sitter-vicuna");
    dir.push("src");

    let mut c_config = cc::Build::new();
    c_config.include(&dir);

    if matches!(
        build_target::target_arch().unwrap(),
        build_target::Arch::WASM32
    ) {
        let mut sysroot = get_repo_root();
        sysroot.push("wasm-stdlib-hack/include/libc");
        c_config.include(&sysroot);
    }

    c_config
        .file(dir.join("parser.c"))
        .compile("tree-sitter-vicuna")
}
