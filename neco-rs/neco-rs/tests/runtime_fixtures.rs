use neco_rs::compile_path_to_elf;
use std::ffi::OsString;
use std::fs;
use std::io::Write;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output, Stdio};
use std::time::{SystemTime, UNIX_EPOCH};

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
    let output = std::env::temp_dir().join(format!("neco-rs-{name}-{unique}"));

    compile_path_to_elf(root, &output).expect("compile fixture");

    let mut permissions = fs::metadata(&output)
        .expect("binary metadata")
        .permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&output, permissions).expect("binary permissions");

    output
}

fn runtime_test_runner(binary: &Path) -> Command {
    let qemu =
        std::env::var_os("NECO_RS_TEST_QEMU").unwrap_or_else(|| OsString::from("qemu-x86_64"));
    let mut command = Command::new(&qemu);
    command.arg(binary);
    command
}

fn run_fixture_status(root: &Path, name: &str) -> std::process::ExitStatus {
    let output = compile_fixture(root, name);
    let status = runtime_test_runner(&output)
        .status()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    fs::remove_file(&output).expect("cleanup binary");
    status
}

fn run_fixture_output(root: &Path, name: &str) -> Output {
    let output = compile_fixture(root, name);
    let run = runtime_test_runner(&output)
        .output()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    fs::remove_file(&output).expect("cleanup binary");
    run
}

fn run_fixture_with_input(root: &Path, name: &str, stdin: &[u8]) -> Output {
    let output = compile_fixture(root, name);
    let mut child = runtime_test_runner(&output)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    child
        .stdin
        .as_mut()
        .expect("child stdin")
        .write_all(stdin)
        .expect("write child stdin");
    let run = child.wait_with_output().expect("collect child output");
    fs::remove_file(&output).expect("cleanup binary");
    run
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
fn compiles_and_runs_u8_array_hello_world_fixture() {
    let root = repo_root().join("tests/testcases/u8-array-hello-world");
    let run = run_fixture_output(&root, "u8-array-hello-world");
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"hello, world\n");
    assert!(run.stderr.is_empty());
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
