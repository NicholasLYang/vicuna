use std::env::current_dir;

fn main() {
    println!("cargo:rerun-if-changed=tree-sitter-vicuna/grammar.js");
    let mut dir = current_dir().unwrap();
    dir.push("tree-sitter-vicuna");
    dir.push("src");

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-vicuna");
}
