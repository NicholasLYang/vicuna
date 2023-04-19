use anyhow::Result;
use clap::Parser;
use std::fs;
use vicuna_compiler::compile;

#[derive(Debug, Clone, Parser)]
#[command(author, version, about)]
enum Command {
    /// Run a Vicuna file
    Run { source_path: String },
    /// Check if a Vicuna file is valid
    Check { source_path: String },
    /// Compile a Vicuna file to JavaScript
    Build { source_path: String },
}

fn build(source_path: String) -> Result<String> {
    let source = fs::read_to_string(source_path)?;
    let output = compile(&source)?;
    for type_error in output.type_errors {
        eprintln!("{:?}", type_error);
    }

    Ok(output.js)
}

fn run(source_path: String) -> Result<()> {
    build(source_path)?;

    Ok(())
}

// TODO: Only type-check and don't compile
fn check(source_path: String) -> Result<()> {
    build(source_path)?;

    Ok(())
}

fn main() -> Result<()> {
    let command = Command::parse();

    match command {
        Command::Run { source_path } => run(source_path)?,
        Command::Check { source_path } => check(source_path)?,
        Command::Build { source_path } => {
            build(source_path)?;
        }
    }

    Ok(())
}
