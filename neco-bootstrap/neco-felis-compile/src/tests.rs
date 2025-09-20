use super::*;
use std::process::{Command, Stdio};
use tempfile::TempDir;

#[test]
fn test_compile_exit_42() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/exit_42.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov rax, 231"));
    assert!(assembly.contains("mov rdi, 42"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_add() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/add.fe").unwrap();
    println!("Generated assembly for add.fe:\n{assembly}");

    // Update expectations based on the new #let syntax
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
    assert!(assembly.contains("sub rsp, 16")); // Stack allocation for 2 let variables
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 0], 231")); // syscall_id = 231u64
    assert!(assembly.contains("mov rax, 40")); // u64_add first arg
    assert!(assembly.contains("mov rbx, 2")); // u64_add second arg
    assert!(assembly.contains("add rax, rbx")); // u64_add operation
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rax")); // Store result
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_compile_sub() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/sub.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov rax, 50"));
    assert!(assembly.contains("mov rbx, 8"));
    assert!(assembly.contains("sub rax, rbx"));
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rax"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_mul() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/mul.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov rax, 6"));
    assert!(assembly.contains("mov rbx, 7"));
    assert!(assembly.contains("imul rax, rbx"));
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rax"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_div() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/div.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov rax, 84"));
    assert!(assembly.contains("mov rbx, 2"));
    assert!(assembly.contains("xor rdx, rdx"));
    assert!(assembly.contains("div rbx"));
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rax"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_mod() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/mod.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov rax, 142"));
    assert!(assembly.contains("mov rbx, 100"));
    assert!(assembly.contains("xor rdx, rdx"));
    assert!(assembly.contains("div rbx"));
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rdx"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

/// Helper function to compile, assemble, link, and execute a Felis program
fn compile_and_execute(
    file_path: &str,
) -> Result<std::process::ExitStatus, Box<dyn std::error::Error>> {
    // Create temporary directory for build artifacts
    let temp_dir = TempDir::new()?;
    let asm_file = temp_dir.path().join("program.s");
    let obj_file = temp_dir.path().join("program.o");
    let exe_file = temp_dir.path().join("program");

    // Step 1: Compile Felis to assembly
    let assembly = compile_file_to_assembly(file_path)?;
    eprintln!("assembly : {assembly}");
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
        .args([
            obj_file.to_string_lossy().as_ref(),
            "-o",
            &exe_file.to_string_lossy(),
        ])
        .status()?;

    if !ld_status.success() {
        return Err("Linking failed".into());
    }

    // Step 4: Execute the program
    let exec_status = Command::new(&exe_file).status()?;

    Ok(exec_status)
}

/// Helper function to compile, assemble, link, and execute a Felis program with output capture
fn compile_and_execute_with_output(
    file_path: &str,
) -> Result<std::process::Output, Box<dyn std::error::Error>> {
    // Create temporary directory for build artifacts
    let temp_dir = TempDir::new()?;
    let asm_file = temp_dir.path().join("program.s");
    let obj_file = temp_dir.path().join("program.o");
    let exe_file = temp_dir.path().join("program");

    // Step 1: Compile Felis to assembly
    let assembly = compile_file_to_assembly(file_path)?;
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
        .args([
            obj_file.to_string_lossy().as_ref(),
            "-o",
            &exe_file.to_string_lossy(),
        ])
        .status()?;

    if !ld_status.success() {
        return Err("Linking failed".into());
    }

    // Step 4: Execute the program and capture output
    let output = Command::new(&exe_file).output()?;

    Ok(output)
}

/// Helper function to compile, assemble, link, and execute a Felis program
fn compile_and_execute_with_ptx(
    file_path: &str,
) -> Result<std::process::ExitStatus, Box<dyn std::error::Error>> {
    // Create temporary directory for build artifacts
    let temp_dir = TempDir::new()?;
    eprintln!("temp_dir = {temp_dir:?}");
    let asm_file = temp_dir.path().join("program.s");
    let obj_file = temp_dir.path().join("program.o");
    let exe_file = temp_dir.path().join("program");

    // Step 1: Compile Felis to assembly
    let assembly = compile_file_to_assembly_with_ptx(file_path)?;
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
    let ld_status = Command::new("gcc")
        .args([
            "-no-pie",
            obj_file.to_string_lossy().as_ref(),
            "-o",
            &exe_file.to_string_lossy(),
            "/opt/cuda/lib64/stubs/libcuda.so",
        ])
        .status()?;

    if !ld_status.success() {
        return Err("Linking failed".into());
    }

    // Step 4: Execute the program
    let exec_status = Command::new(&exe_file).stdout(Stdio::null()).status()?;
    // std::thread::sleep(std::time::Duration::from_secs(50));

    Ok(exec_status)
}

/// Helper function to compile, assemble, link, and execute a Felis program with PTX and output capture
fn compile_and_execute_with_ptx_output(
    file_path: &str,
) -> Result<std::process::Output, Box<dyn std::error::Error>> {
    // Create temporary directory for build artifacts
    let temp_dir = TempDir::new()?;
    eprintln!("temp_dir = {temp_dir:?}");
    let asm_file = temp_dir.path().join("program.s");
    let obj_file = temp_dir.path().join("program.o");
    let exe_file = temp_dir.path().join("program");

    // Step 1: Compile Felis to assembly
    let assembly = compile_file_to_assembly_with_ptx(file_path)?;
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
    let ld_status = Command::new("gcc")
        .args([
            "-no-pie",
            obj_file.to_string_lossy().as_ref(),
            "-o",
            &exe_file.to_string_lossy(),
            "/opt/cuda/lib64/stubs/libcuda.so",
        ])
        .status()?;

    if !ld_status.success() {
        return Err("Linking failed".into());
    }

    // Step 4: Execute the program and capture output
    let output = Command::new(&exe_file).output()?;
    // std::thread::sleep(std::time::Duration::from_secs(50));

    Ok(output)
}

#[test]
fn test_exit_42_integration() {
    let result = compile_and_execute("../../testcases/felis/single/exit_42.fe");

    match result {
        Ok(status) => {
            println!(
                "exit_42.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // exit_42.fe should exit with code 42
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("exit_42.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_let_integration() {
    let result = compile_and_execute("../../testcases/felis/single/let.fe");

    match result {
        Ok(status) => {
            println!(
                "let.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // let.fe should exit with code 42 (error_code value in syscall)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("let.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_add_integration() {
    let result = compile_and_execute("../../testcases/felis/single/add.fe");

    match result {
        Ok(status) => {
            println!(
                "add.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // add.fe should exit with code 42 (40 + 2 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("add.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_sub_integration() {
    let result = compile_and_execute("../../testcases/felis/single/sub.fe");

    match result {
        Ok(status) => {
            println!(
                "sub.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // sub.fe should exit with code 42 (50 - 8 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("sub.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_mul_integration() {
    let result = compile_and_execute("../../testcases/felis/single/mul.fe");

    match result {
        Ok(status) => {
            println!(
                "mul.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // mul.fe should exit with code 42 (6 * 7 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("mul.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_div_integration() {
    let result = compile_and_execute("../../testcases/felis/single/div.fe");

    match result {
        Ok(status) => {
            println!(
                "div.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // div.fe should exit with code 42 (84 / 2 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("div.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_mod_integration() {
    let result = compile_and_execute("../../testcases/felis/single/mod.fe");

    match result {
        Ok(status) => {
            println!(
                "mod.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // mod.fe should exit with code 42 (142 % 100 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("mod.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_compile_add_f32() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/add_f32.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov eax, 0x42200000")); // 40.0f32
    assert!(assembly.contains("movd xmm0, eax"));
    assert!(assembly.contains("mov eax, 0x40000000")); // 2.0f32
    assert!(assembly.contains("movd xmm1, eax"));
    assert!(assembly.contains("addss xmm0, xmm1"));
    assert!(assembly.contains("movss dword ptr [rbp - 8 - 8], xmm0"));
    assert!(assembly.contains("cvttss2si rax, xmm0"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_sub_f32() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/sub_f32.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov eax, 0x42480000")); // 50.0f32
    assert!(assembly.contains("movd xmm0, eax"));
    assert!(assembly.contains("mov eax, 0x41000000")); // 8.0f32
    assert!(assembly.contains("movd xmm1, eax"));
    assert!(assembly.contains("subss xmm0, xmm1"));
    assert!(assembly.contains("movss dword ptr [rbp - 8 - 8], xmm0"));
    assert!(assembly.contains("cvttss2si rax, xmm0"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_mul_f32() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/mul_f32.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov eax, 0x40c00000")); // 6.0f32
    assert!(assembly.contains("movd xmm0, eax"));
    assert!(assembly.contains("mov eax, 0x40e00000")); // 7.0f32
    assert!(assembly.contains("movd xmm1, eax"));
    assert!(assembly.contains("mulss xmm0, xmm1"));
    assert!(assembly.contains("movss dword ptr [rbp - 8 - 8], xmm0"));
    assert!(assembly.contains("cvttss2si rax, xmm0"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_compile_div_f32() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/div_f32.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("mov eax, 0x42a80000")); // 84.0f32
    assert!(assembly.contains("movd xmm0, eax"));
    assert!(assembly.contains("mov eax, 0x40000000")); // 2.0f32
    assert!(assembly.contains("movd xmm1, eax"));
    assert!(assembly.contains("divss xmm0, xmm1"));
    assert!(assembly.contains("movss dword ptr [rbp - 8 - 8], xmm0"));
    assert!(assembly.contains("cvttss2si rax, xmm0"));
    assert!(assembly.contains("syscall"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
}

#[test]
fn test_add_f32_integration() {
    let result = compile_and_execute("../../testcases/felis/single/add_f32.fe");

    match result {
        Ok(status) => {
            println!(
                "add_f32.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // add_f32.fe should exit with code 42 (40.0 + 2.0 = 42.0)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("add_f32.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_sub_f32_integration() {
    let result = compile_and_execute("../../testcases/felis/single/sub_f32.fe");

    match result {
        Ok(status) => {
            println!(
                "sub_f32.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // sub_f32.fe should exit with code 42 (50.0 - 8.0 = 42.0)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("sub_f32.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_mul_f32_integration() {
    let result = compile_and_execute("../../testcases/felis/single/mul_f32.fe");

    match result {
        Ok(status) => {
            println!(
                "mul_f32.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // mul_f32.fe should exit with code 42 (6.0 * 7.0 = 42.0)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("mul_f32.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_div_f32_integration() {
    let result = compile_and_execute("../../testcases/felis/single/div_f32.fe");

    match result {
        Ok(status) => {
            println!(
                "div_f32.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // div_f32.fe should exit with code 42 (84.0 / 2.0 = 42.0)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("div_f32.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_compile_let_mut() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/let_mut.fe").unwrap();
    println!("Generated assembly for let_mut.fe:\n{assembly}");

    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));
    assert!(assembly.contains("sub rsp, 24")); // Stack allocation for 3 variables (syscall_id, value, reference)
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 0], 231")); // syscall_id = 231u64
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], rax")); // let mut error_code = 0u64 (value)
    assert!(assembly.contains("lea rax, qword ptr [rbp - 8 - 8]")); // Calculate address of error_code
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 16], rax")); // Store address in error_code_ref
    assert!(assembly.contains("mov rbx, qword ptr [rbp - 8 - 16]")); // Load address from error_code_ref
    assert!(assembly.contains("mov qword ptr [rbx], rax")); // error_code_ref <- 42u64 (indirect assignment)
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_let_mut_integration() {
    let result = compile_and_execute("../../testcases/felis/single/let_mut.fe");

    match result {
        Ok(status) => {
            println!(
                "let_mut.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // let_mut.fe should exit with code 42 (assigned error_code value in syscall)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("let_mut.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_compile_array() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/array_1.fe").unwrap();
    println!("Generated assembly for array_1.fe:\n{assembly}");

    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("_start:"));

    // Check for mmap syscalls (one for each field: x, y, z)
    assert!(assembly.contains("mov rax, 9")); // sys_mmap
    assert!(assembly.contains("mov rdi, 0")); // addr = NULL
    assert!(assembly.contains("mov rdx, 3")); // prot = PROT_READ | PROT_WRITE
    assert!(assembly.contains("mov r10, 34")); // flags = MAP_PRIVATE | MAP_ANONYMOUS
    assert!(assembly.contains("mov r8, -1")); // fd = -1
    assert!(assembly.contains("mov r9, 0")); // offset = 0

    // Check for array field assignments (f32 literal store path)
    assert!(assembly.contains("mov ebx, 0x41200000")); // 10.0f32
    assert!(assembly.contains("mov dword ptr [rbx], ebx"));

    // Check for field access in builtin calls
    assert!(assembly.contains("movss xmm0, dword ptr [rax]"));
    assert!(assembly.contains("addss xmm0, xmm1"));

    // Check for f32_to_u64 conversion
    assert!(assembly.contains("cvttss2si rax, xmm0"));

    assert!(assembly.contains("syscall"));
}

#[test]
fn test_if_1_integration() {
    let result = compile_and_execute("../../testcases/felis/single/if_1.fe");

    match result {
        Ok(status) => {
            println!(
                "if_1.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // if_1.fe should exit with code 42 (0 == 0 is true, so executes then body)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("if_1.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_if_2_integration() {
    let result = compile_and_execute("../../testcases/felis/single/if_2.fe");

    match result {
        Ok(status) => {
            println!(
                "if_2.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // if_2.fe should exit with code 42 (0 == 1 is false, so executes else body)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("if_2.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_array_integration() {
    // Compile-only check for updated Array syntax
    let assembly = compile_file_to_assembly("../../testcases/felis/single/array_1.fe").unwrap();
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_array_len_integration() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/array_len.fe").unwrap();
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_array_2_integration() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/array_2.fe").unwrap();
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_array_3_integration() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/array_3.fe").unwrap();
    assert!(assembly.contains("syscall"));
}

#[test]
fn test_array_4_integration() {
    let result = compile_and_execute_with_output("../../testcases/felis/single/array_4.fe");

    match result {
        Ok(output) => {
            let expected = "P3\n2 2\n255\n0 1 2\n3 4 5\n6 7 8\n9 10 11\n";
            let actual_output = String::from_utf8_lossy(&output.stdout);
            assert_eq!(actual_output, expected);
        }
        Err(e) => {
            // Skip test if assembler/linker not available
            panic!("Skipping array_4.fe integration test: {e}");
        }
    }
}

#[test]
fn test_loop_break() {
    let result = compile_and_execute("../../testcases/felis/single/loop_break.fe");

    match result {
        Ok(status) => {
            println!(
                "loop_break.fe executed successfully with exit code: {:?}",
                status.code()
            );
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("loop_break.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_compile_proc_call() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/proc_call.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("f:"));
    assert!(assembly.contains("_start:"));

    // Check that proc f is defined and called
    assert!(assembly.contains("call f"));
    assert!(assembly.contains("ret")); // Both main and f should have ret

    // Check that syscall is properly set up
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 0], 231")); // syscall_id = 231u64
    assert!(assembly.contains("syscall"));

    // Check that function arguments are set up
    assert!(assembly.contains("mov rdi, 40")); // First argument
    assert!(assembly.contains("mov rsi, 2")); // Second argument
}

#[test]
fn test_compile_print_c() {
    let assembly = compile_file_to_assembly("../../testcases/felis/single/print_c.fe").unwrap();
    assert!(assembly.contains(".intel_syntax noprefix"));
    assert!(assembly.contains("main:"));
    assert!(assembly.contains("print_c:"));
    assert!(assembly.contains("_start:"));

    // Check that proc print_c is called
    assert!(assembly.contains("call print_c"));

    // Check that let mut with variable assignment works
    assert!(assembly.contains("lea rax, qword ptr [rbp")); // Address calculation for y_ref

    // Check that print_c uses correct syscall (write syscall: 1)
    assert!(assembly.contains("mov rax, 1")); // sys_write
    assert!(assembly.contains("mov rdi, 1")); // stdout file descriptor

    // Check that main calls print_c with ASCII values
    assert!(assembly.contains("mov rdi, 97")); // ASCII 'a'
    assert!(assembly.contains("mov rdi, 10")); // ASCII '\n'

    // Check that program exits with code 0
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 8], 0")); // error_code = 0u64
    assert!(assembly.contains("mov qword ptr [rbp - 8 - 0], 231")); // syscall_id = 231u64
}

#[test]
fn test_proc_call_integration() {
    let result = compile_and_execute("../../testcases/felis/single/proc_call.fe");

    match result {
        Ok(status) => {
            println!(
                "proc_call.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // proc_call.fe should exit with code 42 (40 + 2 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("proc_call.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_print_c_integration() {
    let result = compile_and_execute_with_output("../../testcases/felis/single/print_c.fe");

    match result {
        Ok(output) => {
            println!("print_c.fe executed successfully");
            // println!("stdout: {:?}", String::from_utf8_lossy(&output.stdout));
            println!("stderr: {:?}", String::from_utf8_lossy(&output.stderr));
            println!("exit code: {:?}", output.status.code());

            // Check that the program exits with code 0
            assert_eq!(
                output.status.code(),
                Some(0),
                "Program should exit with code 0"
            );

            // Check that stdout is "a\n" (ASCII 97 + ASCII 10)
            let expected_output = "a\n";
            let actual_output = String::from_utf8_lossy(&output.stdout);
            assert_eq!(
                actual_output, expected_output,
                "Program should output 'a\\n'"
            );
        }
        Err(e) => {
            panic!("print_c.fe integration test failed: {e}");
        }
    }
}

#[test]
fn test_print_num3_integration() {
    let result = compile_and_execute_with_output("../../testcases/felis/single/print_num3.fe");

    match result {
        Ok(output) => {
            println!("print_num3.fe executed successfully");
            // println!("stdout: {:?}", String::from_utf8_lossy(&output.stdout));
            println!("stderr: {:?}", String::from_utf8_lossy(&output.stderr));
            println!("exit code: {:?}", output.status.code());

            // Check that the program exits with code 0
            assert_eq!(
                output.status.code(),
                Some(0),
                "Program should exit with code 0"
            );

            // Check that stdout is "123\n12\n1\n0\n"
            let expected_output = "123\n12\n1\n0\n";
            let actual_output = String::from_utf8_lossy(&output.stdout);
            assert_eq!(
                actual_output, expected_output,
                "Program should output 'a\\n'"
            );
        }
        Err(e) => {
            panic!("print_num3.fe integration test failed: {e}");
        }
    }
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_1() {
    let result = compile_and_execute_with_ptx("../../testcases/felis/single/ptx_1.fe");

    match result {
        Ok(status) => {
            println!(
                "ptx_1.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // ptx_1.fe should exit with code 42
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("ptx_1.fe integration test failed: {e}");
        }
    }
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_2() {
    let result = compile_and_execute_with_ptx("../../testcases/felis/single/ptx_2.fe");

    match result {
        Ok(status) => {
            println!(
                "ptx_2.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // ptx_2.fe should exit with code 42
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("ptx_2.fe integration test failed: {e}");
        }
    }
}

#[test]
#[ignore]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_2_output() {
    let result = compile_and_execute_with_ptx_output("../../testcases/felis/single/ptx_2.fe");

    match result {
        Ok(output) => {
            println!("ptx_2.fe executed successfully");
            // println!("stdout: {:?}", String::from_utf8_lossy(&output.stdout));
            println!("stderr: {:?}", String::from_utf8_lossy(&output.stderr));
            println!("exit code: {:?}", output.status.code());

            // Check that the program exits with code 42
            assert_eq!(
                output.status.code(),
                Some(42),
                "Program should exit with code 42"
            );

            // Check that stdout matches expected output
            // The program outputs the value at ps.r[0], which should be 0 for thread_id 0
            let expected_output = "0";
            let actual_output = String::from_utf8_lossy(&output.stdout);
            // eprintln!("actual_output = {actual_output:?}");
            let _ = std::fs::write("/tmp/a.ppm", actual_output.as_bytes());
            assert_eq!(
                actual_output, expected_output,
                "Program output should match expected output"
            );
        }
        Err(e) => {
            panic!("Skipping ptx_2.fe output integration test: {e}");
        }
    }
}

#[test]
#[ignore]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_3_output() {
    let result = compile_and_execute_with_ptx_output("../../testcases/felis/single/ptx_3.fe");

    match result {
        Ok(output) => {
            println!("ptx_3.fe executed successfully");
            // println!("stdout: {:?}", String::from_utf8_lossy(&output.stdout));
            println!("stderr: {:?}", String::from_utf8_lossy(&output.stderr));
            println!("exit code: {:?}", output.status.code());

            // Check that the program exits with code 42
            assert_eq!(
                output.status.code(),
                Some(42),
                "Program should exit with code 42"
            );

            // Check that stdout matches expected output
            // The program outputs the value at ps.r[0], which should be 0 for thread_id 0
            let expected_output = "P3\n2 2\n255\n0 0 51\n255 0 51\n0 255 51\n255 255 51\n";
            let actual_output = String::from_utf8_lossy(&output.stdout);
            // eprintln!("actual_output = {actual_output:?}");
            // std::fs::write("/tmp/a.ppm", actual_output.as_bytes());
            assert_eq!(
                actual_output, expected_output,
                "Program output should match expected output"
            );
        }
        Err(e) => {
            panic!("Skipping ptx_3.fe output integration test: {e}");
        }
    }
}

#[test]
fn test_compile_struct_3() {
    let result = compile_file_to_assembly("../../testcases/felis/single/struct_3.fe");
    match result {
        Ok(assembly) => {
            println!("Generated assembly for struct_3.fe:\n{assembly}");

            // Basic assembly structure checks
            assert!(assembly.contains(".intel_syntax noprefix"));
            assert!(assembly.contains("main:"));
            assert!(assembly.contains("_start:"));
            assert!(assembly.contains("syscall"));
        }
        Err(e) => {
            panic!("Compilation failed: {e}");
        }
    }
}

#[test]
fn test_struct_3_integration() {
    let result = compile_and_execute("../../testcases/felis/single/struct_3.fe");

    match result {
        Ok(status) => {
            println!(
                "struct_3.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // struct_3.fe should exit with code 42 (18 + 14 + 10 = 42)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("struct_3.fe integration test failed: {e}");
        }
    }
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_4() {
    let result = compile_and_execute_with_ptx("../../testcases/felis/single/ptx_4.fe");

    match result {
        Ok(status) => {
            println!(
                "ptx_4.fe executed successfully with exit code: {:?}",
                status.code()
            );
            // ptx_4.fe should exit with code 42
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("ptx_4.fe integration test failed: {e}");
        }
    }
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_f32_zero_compile_only() {
    let assembly =
        compile_file_to_assembly_with_ptx("../../testcases/felis/single/ptx_f32_zero.fe")
            .expect("should compile with PTX");
    // Kernel should be embedded and contain f32 subtraction with 42.0 - 0.0
    assert!(assembly.contains("ptx_code_kernel:"));
    assert!(assembly.contains(".visible .entry kernel("));
    // Check the immediate bits for 42.0f32 and 0.0f32, and the sub.f32 op
    assert!(assembly.contains("0x42280000"));
    assert!(assembly.contains("0x00000000"));
    assert!(assembly.contains("sub.f32"));
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_f32_zero_execute() {
    let result = compile_and_execute_with_ptx("../../testcases/felis/single/ptx_f32_zero.fe");

    match result {
        Ok(status) => {
            // Program should exit with code 42 (42.0 - 0.0)
            assert_eq!(status.code(), Some(42), "Program should exit with code 42");
        }
        Err(e) => {
            panic!("ptx_f32_zero.fe integration test failed: {e}");
        }
    }
}

#[test]
#[cfg(feature = "has-ptx-device")]
fn test_ptx_proc_call_compile_only() {
    let assembly =
        compile_file_to_assembly_with_ptx("../../testcases/felis/single/ptx_proc_call.fe")
            .expect("should compile with PTX");
    // PTX code for kernel 'f' should be embedded
    assert!(assembly.contains("ptx_code_f:"));
    assert!(assembly.contains(".visible .entry f("));
    // Helper PTX proc should not be emitted as a kernel
    assert!(!assembly.contains("ptx_code_vec3_mul:"));
}
