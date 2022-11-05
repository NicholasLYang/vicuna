use std::env::current_dir;

fn main() {
    let mut dir = current_dir().unwrap();
    dir.push("tree-sitter-vicuna");
    dir.push("src");

    cc::Build::new()
        .include(&dir)
        .file(dir.join("parser.c"))
        .compile("tree-sitter-vicuna");
}
