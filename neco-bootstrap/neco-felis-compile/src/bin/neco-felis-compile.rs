use neco_felis_compile::compile_file_to_assembly;
use std::process::Command;
use tempfile::TempDir;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<_> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <source.fe> [-o <output>]", args[0]);
        std::process::exit(1);
    }

    // Parse command line arguments
    let mut source_file = None;
    let mut output_file = None;
    let mut i = 1;

    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                if i + 1 < args.len() {
                    output_file = Some(args[i + 1].clone());
                    i += 2;
                } else {
                    eprintln!("Error: -o requires an argument");
                    std::process::exit(1);
                }
            }
            _ => {
                if source_file.is_none() {
                    source_file = Some(args[i].clone());
                } else {
                    eprintln!("Error: Multiple source files specified");
                    std::process::exit(1);
                }
                i += 1;
            }
        }
    }

    let source_file = source_file.ok_or("No source file specified")?;

    // Compile to assembly
    let assembly = compile_file_to_assembly(&source_file)?;

    // If no output file specified, print assembly to stdout
    if output_file.is_none() {
        println!("{assembly}");
        return Ok(());
    }

    // Generate binary
    let output_file = output_file.unwrap();
    generate_binary(&assembly, &output_file)?;

    Ok(())
}

fn generate_binary(assembly: &str, output_file: &str) -> Result<(), Box<dyn std::error::Error>> {
    // Create temporary directory for build artifacts
    let temp_dir = TempDir::new()?;
    let asm_file = temp_dir.path().join("program.s");
    let obj_file = temp_dir.path().join("program.o");

    // Step 1: Write assembly to file
    std::fs::write(&asm_file, assembly)?;

    // Step 2: Assemble to object file
    let as_status = Command::new("as")
        .args([
            "--64",
            &asm_file.to_string_lossy(),
            "-o",
            &obj_file.to_string_lossy(),
        ])
        .status()?;

    if !as_status.success() {
        return Err("Assembly failed".into());
    }

    // Step 3: Link to executable
    let ld_status = Command::new("ld")
        .args([obj_file.to_string_lossy().as_ref(), "-o", output_file])
        .status()?;

    if !ld_status.success() {
        return Err("Linking failed".into());
    }

    eprintln!("Binary generated: {output_file}");
    Ok(())
}
