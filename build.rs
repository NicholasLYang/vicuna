use std::env::current_dir;

fn main() {
    println!("cargo:rerun-if-changed=tree-sitter-vicuna/grammar.js");
    let mut dir = current_dir().unwrap();
    dir.push("tree-sitter-vicuna");
    dir.push("src");

    let mut sysroot = current_dir().unwrap();
    sysroot.push("wasm-stdlib-hack/include/libc");
    cc::Build::new()
        .include(&dir)
        .include(&sysroot)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-vicuna")
}
