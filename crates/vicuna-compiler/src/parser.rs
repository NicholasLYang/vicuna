use anyhow::{anyhow, Result};
use tree_sitter_c2rust::{Language, Parser, Tree};

extern "C" {
    fn tree_sitter_vicuna() -> Language;
}

pub fn parse(source: &str) -> Result<Tree> {
    let mut parser = Parser::new();
    parser.set_language(unsafe { tree_sitter_vicuna() })?;

    let tree = parser
        .parse(&source, None)
        .ok_or_else(|| anyhow!("Unable to parse code"))?;

    Ok(tree)
}
