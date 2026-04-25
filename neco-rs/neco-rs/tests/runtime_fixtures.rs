use neco_rs::compile_path_to_elf;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

static FIXTURE_EXEC_LOCK: Mutex<()> = Mutex::new(());

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root")
}

fn compile_fixture(root: &Path, name: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    let output_dir = std::env::temp_dir().join(format!("neco-rs-build-{name}-{unique}"));
    fs::create_dir_all(&output_dir).expect("create build temp dir");
    let output = output_dir.join(name);

    compile_path_to_elf(root, &output).expect("compile fixture");

    output
}

fn runtime_temp_dir(name: &str) -> PathBuf {
    let unique = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("system time")
        .as_nanos();
    let dir = std::env::temp_dir().join(format!("neco-rs-runtime-{name}-{unique}"));
    fs::create_dir_all(&dir).expect("create runtime temp dir");
    dir
}

fn runtime_test_runner(binary: &Path) -> Command {
    if let Some(qemu) = std::env::var_os("NECO_RS_TEST_QEMU") {
        let mut command = Command::new(qemu);
        command.arg(binary);
        return command;
    }

    if cfg!(all(target_os = "linux", target_arch = "x86_64")) {
        return Command::new(binary);
    }

    panic!(
        "generated fixture binaries are Linux x86_64 ELF files; set NECO_RS_TEST_QEMU to run them on this host"
    );
}

fn compile_and_spawn_fixture(
    root: &Path,
    name: &str,
    configure: impl FnOnce(&mut Command),
) -> (PathBuf, Child) {
    let _guard = FIXTURE_EXEC_LOCK.lock().expect("fixture exec lock");
    let output = compile_fixture(root, name);
    let mut command = runtime_test_runner(&output);
    configure(&mut command);
    let child = command
        .spawn()
        .unwrap_or_else(|error| panic!("run fixture binary: {error}"));
    (output, child)
}

fn cleanup_fixture_binary(output: &Path) {
    fs::remove_file(output).expect("cleanup binary");
    fs::remove_dir(output.parent().expect("build temp dir")).expect("cleanup build temp dir");
}

fn run_fixture_status(root: &Path, name: &str) -> std::process::ExitStatus {
    let (output, mut child) = compile_and_spawn_fixture(root, name, |_| {});
    let status = child.wait().expect("collect child status");
    cleanup_fixture_binary(&output);
    status
}

fn run_fixture_output(root: &Path, name: &str) -> Output {
    let (output, child) = compile_and_spawn_fixture(root, name, |command| {
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
    });
    let run = child.wait_with_output().expect("collect child output");
    cleanup_fixture_binary(&output);
    run
}

fn run_fixture_output_with_args(root: &Path, name: &str, args: &[&str]) -> Output {
    let (output, child) = compile_and_spawn_fixture(root, name, |command| {
        command
            .args(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
    });
    let run = child.wait_with_output().expect("collect child output");
    cleanup_fixture_binary(&output);
    run
}

fn run_fixture_output_in_dir(root: &Path, name: &str, current_dir: &Path) -> Output {
    let (output, child) = compile_and_spawn_fixture(root, name, |command| {
        command
            .current_dir(current_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
    });
    let run = child.wait_with_output().expect("collect child output");
    cleanup_fixture_binary(&output);
    run
}

fn run_fixture_with_input(root: &Path, name: &str, stdin: &[u8]) -> Output {
    let (output, mut child) = compile_and_spawn_fixture(root, name, |command| {
        command
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
    });
    child
        .stdin
        .as_mut()
        .expect("child stdin")
        .write_all(stdin)
        .expect("write child stdin");
    let run = child.wait_with_output().expect("collect child output");
    cleanup_fixture_binary(&output);
    run
}

fn run_neco_felis_fixture(input_root: &Path, temp_name: &str) -> (Output, Vec<u8>, Output) {
    let root = repo_root().join("neco-felis");
    let temp_dir = runtime_temp_dir(temp_name);

    let (binary, child) = compile_and_spawn_fixture(&root, "neco-felis", |command| {
        command
            .current_dir(&temp_dir)
            .arg(input_root)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
    });
    let run = child
        .wait_with_output()
        .expect("collect neco-felis fixture output");
    let emitted = temp_dir.join("a.out");
    let emitted_bytes = fs::read(&emitted).expect("read emitted a.out");
    let emitted_child = {
        let _guard = FIXTURE_EXEC_LOCK.lock().expect("fixture exec lock");
        let mut command = runtime_test_runner(&emitted);
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
        command
            .spawn()
            .unwrap_or_else(|error| panic!("run emitted a.out: {error}"))
    };
    let emitted_run = emitted_child
        .wait_with_output()
        .expect("collect emitted a.out output");

    cleanup_fixture_binary(&binary);
    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

    (run, emitted_bytes, emitted_run)
}

#[test]
fn compiles_and_runs_i32_ops_fixture() {
    let root = repo_root().join("tests/testcases/i32-ops");
    let status = run_fixture_status(&root, "i32-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_u8_ops_fixture() {
    let root = repo_root().join("tests/testcases/u8-ops");
    let status = run_fixture_status(&root, "u8-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_array_basic_fixture() {
    let root = repo_root().join("tests/testcases/array-basic");
    let status = run_fixture_status(&root, "array-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_array_type_annotation_fixture() {
    let root = repo_root().join("tests/testcases/array-type-annotation");
    let status = run_fixture_status(&root, "array-type-annotation");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_dyn_array_type_annotation_fixture() {
    let root = repo_root().join("tests/testcases/dyn-array-type-annotation");
    let status = run_fixture_status(&root, "dyn-array-type-annotation");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_dyn_array_u8_helpers_fixture() {
    let root = repo_root().join("tests/testcases/dyn-array-u8-helpers");
    let status = run_fixture_status(&root, "dyn-array-u8-helpers");
    assert_eq!(status.code(), Some(99));
}

#[test]
fn compiles_and_runs_dyn_array_len_fixture() {
    let root = repo_root().join("tests/testcases/dyn-array-len");
    let status = run_fixture_status(&root, "dyn-array-len");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_string_basic_fixture() {
    let root = repo_root().join("tests/testcases/string-basic");
    let status = run_fixture_status(&root, "string-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_array_byte_scan_fixture() {
    let root = repo_root().join("tests/testcases/array-byte-scan");
    let status = run_fixture_status(&root, "array-byte-scan");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_workspace_runtime_dependency_fixture() {
    let root = repo_root().join("tests/testcases/workspace-runtime-dependency/workspace-app");
    let status = run_fixture_status(&root, "workspace-runtime-dependency");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_workspace_type_dependency_fixture() {
    let root = repo_root().join("tests/testcases/workspace-type-dependency/workspace-app");
    let status = run_fixture_status(&root, "workspace-type-dependency");
    assert_eq!(status.code(), Some(101));
}

#[test]
fn compiles_and_runs_i32_reference_annotation_fixture() {
    let root = repo_root().join("tests/testcases/i32-reference-annotation");
    let status = run_fixture_status(&root, "i32-reference-annotation");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_proc_reference_annotation_fixture() {
    let root = repo_root().join("tests/testcases/proc-reference-annotation");
    let status = run_fixture_status(&root, "proc-reference-annotation");
    assert_eq!(status.code(), Some(43));
}

#[test]
fn compiles_and_runs_proc_return_fixture() {
    let root = repo_root().join("tests/testcases/proc-return");
    let status = run_fixture_status(&root, "proc-return");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_proc_cli_arg_reference_fixture() {
    let root = repo_root().join("tests/testcases/proc-cli-arg-reference");
    let (output, mut child) =
        compile_and_spawn_fixture(&root, "proc-cli-arg-reference", |command| {
            command.arg("15");
        });
    let run = child.wait().expect("collect child status");
    cleanup_fixture_binary(&output);
    assert_eq!(run.code(), Some(102));
}

#[test]
fn compiles_and_runs_u8_array_hello_world_fixture() {
    let root = repo_root().join("tests/testcases/u8-array-hello-world");
    let run = run_fixture_output(&root, "u8-array-hello-world");
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"hello, world\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_hex_literals_fixture() {
    let root = repo_root().join("tests/testcases/hex-literals");
    let run = run_fixture_output(&root, "hex-literals");
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"A\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_comments_basic_fixture() {
    let root = repo_root().join("tests/testcases/comments-basic");
    let run = run_fixture_output(&root, "comments-basic");
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"Hello, world!\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_enum_match_basic_fixture() {
    let root = repo_root().join("tests/testcases/enum-match-basic");
    let status = run_fixture_status(&root, "enum-match-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_declaration_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-declaration");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_field_access_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-field-access");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_rc_field_access_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-rc-field-access");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_match_payload_single_fixture() {
    let root = repo_root().join("tests/testcases/enum-match-payload");
    let status = run_fixture_status(&root, "enum-match-payload-single");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_match_payload_pair_fixture() {
    let root = repo_root().join("tests/testcases/enum-match-payload");
    let status = run_fixture_status(&root, "enum-match-payload-pair");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_single_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-single");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_pair_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-pair");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_list_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-list");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_proc_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-proc");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_parser_basic_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-parser-basic");
    let status = run_fixture_status(&root, "type-rc-parser-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_stdin_to_stdout_fixture() {
    let root = repo_root().join("tests/testcases/stdin-to-stdout");
    let run = run_fixture_with_input(&root, "stdin-to-stdout", b"echo through stdin\n");
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"echo through stdin\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_open_read_close_fixture() {
    let root = repo_root().join("tests/testcases/open-read-close");
    let run = run_fixture_output_in_dir(&root, "open-read-close", &root);
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"open/read/close fixture\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_open_array_path_fixture() {
    let root = repo_root().join("tests/testcases/open-array-path");
    let run = run_fixture_output_in_dir(&root, "open-array-path", &root);
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"open/array/path fixture\n");
    assert!(run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_open_write_close_fixture() {
    let root = repo_root().join("tests/testcases/open-write-close");
    let temp_dir = runtime_temp_dir("open-write-close");
    let run = run_fixture_output_in_dir(&root, "open-write-close", &temp_dir);
    let written = fs::read(temp_dir.join("created.txt")).expect("read written file");
    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(written, b"open/write/close fixture\n");
}

#[test]
fn compiles_and_runs_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/exit-42");
    let (run, emitted_bytes, emitted_run) = run_neco_felis_fixture(&input_path, "neco-felis");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(42));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_hello_world_with_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/hello-world");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-hello-world");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(0));
    assert_eq!(emitted_run.stdout, b"Hello, world!\n");
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_i32_ops_with_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/i32-ops");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-i32-ops");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(42));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_i32_ops_with_neco_felis_fixture_returns_42() {
    let input_path = repo_root().join("tests/testcases/i32-ops");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-i32-ops-expected-42");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(42));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_i32_exit_local_with_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/i32-exit-local");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-i32-exit-local");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(3));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_i32_add_exit_local_with_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/i32-add-exit-local");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-i32-add-exit-local");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(10));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
fn compiles_and_runs_fn_call_fixture() {
    let root = repo_root().join("tests/testcases/fn-call");
    let status = run_fixture_status(&root, "fn-call");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_true_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_false_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-false");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_else_true_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-else-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_else_false_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-else-false");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_else_if_true_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-else-if-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_if_else_if_fallback_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-else-if-fallback");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_loop_fixture() {
    let root = repo_root().join("tests/testcases/loop");
    let status = run_fixture_status(&root, "loop");
    assert_eq!(status.code(), Some(55));
}

#[test]
fn compiles_and_runs_continue_fixture() {
    let root = repo_root().join("tests/testcases/continue");
    let status = run_fixture_status(&root, "continue");
    assert_eq!(status.code(), Some(50));
}

#[test]
fn compiles_and_runs_cli_args_fixture() {
    let root = repo_root().join("tests/testcases/cli-args");
    let temp_dir = runtime_temp_dir("cli-args");
    let input_path = temp_dir.join("argv-input.txt");
    fs::write(&input_path, b"via argv\n").expect("write cli args input file");

    let run = run_fixture_output_with_args(
        &root,
        "cli-args",
        &[input_path.to_str().expect("utf-8 temp path"), "15"],
    );

    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

    assert_eq!(run.status.code(), Some(102));
    assert_eq!(run.stdout, b"via argv\n");
    assert!(run.stderr.is_empty());
}
