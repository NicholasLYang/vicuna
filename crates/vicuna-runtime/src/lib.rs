use anyhow::Result;
use std::path::Path;
use std::process::Command;
use which::which;

pub fn execute_file(file_path: &Path) -> Result<()> {
    println!("=================");
    let exit_status = Command::new(which("node")?)
        .arg(file_path)
        .spawn()?
        .wait()?;
    println!("=================");
    if exit_status.success() {
        Ok(())
    } else {
        Err(anyhow::anyhow!("execution failed"))
    }
}
