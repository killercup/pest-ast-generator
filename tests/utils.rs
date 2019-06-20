#![allow(unused)]

macro_rules! test {
    ($name:ident : $grammar:expr) => {
        #[test]
        fn $name() {
            crate::utils::compile(stringify!($name), $grammar).unwrap();
        }
    };
}

pub fn compile(name: &str, grammar: &str) -> Result<(), Box<dyn std::error::Error>> {
    use std::{fs, path::PathBuf, process::Command};

    let _ = env_logger::builder().is_test(true).filter(None, log::LevelFilter::Debug).try_init();

    let target_dir = PathBuf::from("./tests/test-package");
    let example_dir = target_dir.join("examples");
    fs::create_dir_all(&example_dir)?;

    let mut rust_code = pest_ast_generator::grammar_to_rust(name, grammar)?.to_string();
    rust_code.push_str("\n\n fn main() {}");

    fs::write(example_dir.join(name).with_extension("rs"), rust_code)?;

    let fmt = Command::new("cargo").args(&["fmt", "--all"]).current_dir(&target_dir).status()?;

    if !fmt.success() {
        Err("cargo fmt failed")?;
    }

    let fmt = std::process::Command::new("cargo")
        .args(&["check", "--example", &name])
        .current_dir(&target_dir)
        .status()?;

    if !fmt.success() {
        Err("cargo check failed")?;
    }

    Ok(())
}
