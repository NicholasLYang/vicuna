use anyhow::{anyhow, Result};
use clap::Parser;
use colored::Colorize;
use miette::Report;
use std::fs;
use std::path::{Path, PathBuf};
use vicuna_compiler::Diagnostic;

#[derive(Debug, Clone, Parser)]
#[command(author, version, about)]
enum Command {
    /// Run a Vicuna file
    Run { source_path: PathBuf },
    /// Check if a Vicuna file is valid
    Check { source_path: PathBuf },
    /// Compile a Vicuna file to JavaScript
    Build { source_path: PathBuf },
}

fn print_diagnostics(diagnostics: Vec<Diagnostic>) {
    for diagnostic in diagnostics {
        eprintln!("{:?}", Report::new(diagnostic));
    }
}

fn build(source_path: &Path) -> Result<PathBuf> {
    let output = vicuna_compiler::compile(&source_path)?;
    println!("{} {}", "Compiled".blue().bold(), source_path.display());

    let js = output
        .js
        .ok_or(anyhow!("could not run due to compilation errors"))?;

    for (path, output) in js {
        let output_path = path.with_extension("v.js");
        fs::write(path, output)?;
        println!("{} {}", "Emitted".blue().bold(), output_path.display());
    }

    Ok(source_path.with_extension("v.js"))
}

fn run(source_path: &Path) -> Result<()> {
    let output_path = build(source_path)?;
    match vicuna_runtime::execute_file(&output_path) {
        Ok(()) => {
            println!("{}", "Done".blue().bold());
        }
        Err(e) => {
            eprintln!("error: {}", e);
        }
    }

    Ok(())
}

fn check(source_path: &Path) -> Result<()> {
    let diagnostics = vicuna_compiler::check(source_path);
    if diagnostics.is_empty() {
        println!("No errors found");
        return Ok(());
    }

    print_diagnostics(diagnostics);

    Ok(())
}

fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    let command = Command::parse();

    match command {
        Command::Run { source_path } => run(&source_path)?,
        Command::Check { source_path } => check(&source_path)?,
        Command::Build { source_path } => {
            build(&source_path)?;
        }
    }

    Ok(())
}
