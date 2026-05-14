use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::codegen::ProgramImage;
use crate::{Error, Result};

pub(crate) fn link_linux_x86_64_libc_start_executable(
    image: &ProgramImage,
    libraries: &[String],
    output: &Path,
) -> Result<()> {
    let work_dir = link_work_dir(output)?;
    fs::create_dir_all(&work_dir).map_err(|source| Error::Io {
        path: Some(work_dir.clone()),
        source,
    })?;

    let assembly_path = work_dir.join("program.s");
    let linker_script_path = work_dir.join("neco.ld");
    fs::write(&assembly_path, program_assembly(image)).map_err(|source| Error::Io {
        path: Some(assembly_path.clone()),
        source,
    })?;
    fs::write(&linker_script_path, linker_script()).map_err(|source| Error::Io {
        path: Some(linker_script_path.clone()),
        source,
    })?;

    let cc = std::env::var_os("CC").unwrap_or_else(|| "cc".into());
    let mut command = Command::new(cc);
    command
        .arg("-no-pie")
        .arg(&assembly_path)
        .arg(format!("-Wl,-T,{}", linker_script_path.display()))
        .arg("-Wl,--no-as-needed");
    for library in libraries {
        command.arg(format!("-l{library}"));
    }
    command.arg("-o").arg(output);

    let run = command
        .output()
        .map_err(|source| Error::Io { path: None, source })?;
    if !run.status.success() {
        return Err(Error::Link {
            status: run.status.code(),
            stderr: String::from_utf8_lossy(&run.stderr).into_owned(),
        });
    }

    let _ = fs::remove_file(&assembly_path);
    let _ = fs::remove_file(&linker_script_path);
    let _ = fs::remove_dir(&work_dir);
    Ok(())
}

fn link_work_dir(output: &Path) -> Result<PathBuf> {
    let parent = output.parent().unwrap_or_else(|| Path::new("."));
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|duration| duration.as_nanos())
        .unwrap_or(0);
    Ok(parent.join(format!(".neco-link-{unique}")))
}

fn program_assembly(image: &ProgramImage) -> String {
    let mut assembly = String::new();
    assembly.push_str(".global main\n");
    assembly.push_str(".type main,@function\n");
    assembly.push_str(".section .text.neco,\"ax\",@progbits\n");
    assembly.push_str("main:\n");
    push_byte_directives(&mut assembly, &image.code);
    assembly.push_str(".size main, .-main\n");

    if !image.data.is_empty() {
        assembly.push_str(".section .rodata.neco,\"a\",@progbits\n");
        assembly.push_str(".balign 1\n");
        push_byte_directives(&mut assembly, &image.data);
    }

    if image.requires_argv {
        assembly.push_str(".section .bss.neco,\"aw\",@nobits\n");
        assembly.push_str(".balign 8\n");
        assembly.push_str(".zero 8\n");
    }

    assembly.push_str(".section .note.GNU-stack,\"\",@progbits\n");
    assembly
}

fn push_byte_directives(assembly: &mut String, bytes: &[u8]) {
    for chunk in bytes.chunks(16) {
        assembly.push_str(".byte ");
        for (index, byte) in chunk.iter().enumerate() {
            if index > 0 {
                assembly.push(',');
            }
            assembly.push_str(&byte.to_string());
        }
        assembly.push('\n');
    }
}

fn linker_script() -> &'static str {
    r#"SECTIONS {
  . = 0x401000;
  .text : { *(.text.neco) *(.text .text.*) }
  . = 0x410000;
  .rodata : { *(.rodata.neco) *(.rodata .rodata.*) }
  . = 0x420000;
  .bss : { *(.bss.neco) *(.bss .bss.* COMMON) }
} INSERT AFTER .interp;
"#
}
