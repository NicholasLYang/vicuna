use anyhow::Result;
use std::fs;
use vicuna_compiler::compile;

fn main() -> Result<()> {
    let arg = std::env::args()
        .nth(1)
        .ok_or_else(|| anyhow::anyhow!("No argument provided"))?;
    let source = fs::read_to_string(arg)?;
    let output = compile(&source)?;

    for type_error in output.type_errors {
        eprintln!("{:?}", type_error);
    }

    println!("{}", output.js);

    Ok(())
}
