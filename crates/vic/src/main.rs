use anyhow::{anyhow, Result};
use camino::{Utf8Path, Utf8PathBuf};
use clap::Parser;
use colored::Colorize;
use miette::Report;
use std::fs;
use vicuna_compiler::Diagnostic;

#[derive(Debug, Clone, Parser)]
#[command(author, version, about)]
enum Command {
    /// Run a Vicuna file
    Run { source_path: Utf8PathBuf },
    /// Check if a Vicuna file is valid
    Check { source_path: Utf8PathBuf },
    /// Compile a Vicuna file to JavaScript
    Build { source_path: Utf8PathBuf },
}

fn print_diagnostics(diagnostics: Vec<Diagnostic>) {
    for diagnostic in diagnostics {
        eprintln!("{:?}", Report::new(diagnostic));
    }
}

fn build(source_path: &Utf8Path) -> Result<Utf8PathBuf> {
    let output = vicuna_compiler::compile(&source_path)?;
    println!("{} {}", "Compiled".blue().bold(), source_path);

    let js = output
        .js
        .ok_or(anyhow!("could not run due to compilation errors"))?;

    for (path, output) in js {
        let output_path = path.with_extension("v.mjs");
        fs::write(&output_path, output)?;
        println!("{} {}", "Emitted".blue().bold(), output_path);
    }

    Ok(source_path.with_extension("v.mjs"))
}

fn run(source_path: &Utf8Path) -> Result<()> {
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

fn check(source_path: &Utf8Path) -> Result<()> {
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
