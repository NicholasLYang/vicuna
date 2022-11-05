use tree_sitter::{Parser, Language, Tree};
use anyhow::Result;
mod ast;

extern "C" { fn tree_sitter_vicuna() -> Language; }

fn main() -> Result<()> {
    let tree = parse_into_cst("2 - -5")?.unwrap();
    println!("{}", tree.root_node().to_sexp());

    Ok(())
}

fn parse_into_cst(source: &str) -> Result<Option<Tree>> {
    let mut parser = Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() }).unwrap();
    Ok(parser.parse(source, None))
}
