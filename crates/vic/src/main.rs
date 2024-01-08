use anyhow::{anyhow, Result};
use clap::Parser;
use colored::Colorize;
use miette::{Error, Report};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use vicuna_compiler::Errors;

#[derive(Debug, Clone, Parser)]
#[command(author, version, about)]
enum Command {
    /// Run a Vicuna file
    Run { source_path: PathBuf },
    /// Check if a Vicuna file is valid
    Check { source_path: PathBuf },
    /// Compile a Vicuna file to JavaScript
    Build { source_path: PathBuf },
    /// Show dependency graph of Vicuna files
    Graph { source_path: PathBuf },
}

fn print_errors(source: String, errors: Errors) {
    let shared_source = Arc::new(source);

    for parse_error in errors.parse_errors {
        eprintln!(
            "{:?}",
            Report::new(parse_error).with_source_code(shared_source.clone())
        );
    }

    for type_error in errors.type_errors {
        eprintln!(
            "{:?}",
            Error::new(type_error).with_source_code(shared_source.clone())
        );
    }
}

fn build(source_path: &Path) -> Result<PathBuf> {
    let source = fs::read_to_string(source_path)?;
    let output = vicuna_compiler::compile(&source)?;
    println!("{} {}", "Compiled".blue().bold(), source_path.display());

    let output_path = source_path.with_extension("v.js");
    print_errors(source, output.errors);
    let js = output
        .js
        .ok_or(anyhow!("could not run due to compilation errors"))?;

    fs::write(&output_path, js)?;
    println!("{} {}", "Emitted".blue().bold(), output_path.display());

    Ok(output_path)
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

fn graph(source_path: &Path) -> Result<()> {
    let mut resolver = vicuna_compiler::Resolver::new(source_path.to_path_buf());
    resolver.build();
    Ok(())
}

fn check(source_path: &Path) -> Result<()> {
    let source = fs::read_to_string(source_path)?;
    let errors = vicuna_compiler::check(&source);
    if errors.parse_errors.is_empty() && errors.type_errors.is_empty() {
        println!("No errors found");
        return Ok(());
    }

    print_errors(source, errors);

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
        Command::Graph { source_path } => graph(&source_path)?,
    }

    Ok(())
}
