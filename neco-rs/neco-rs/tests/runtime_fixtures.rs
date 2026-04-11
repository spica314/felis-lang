use neco_rs::compile_path_to_elf;
use std::ffi::OsString;
use std::fs;
use std::io::Write;
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

fn run_fixture_output_with_args(root: &Path, name: &str, args: &[&str]) -> Output {
    let output = compile_fixture(root, name);
    let run = runtime_test_runner(&output)
        .args(args)
        .output()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    fs::remove_file(&output).expect("cleanup binary");
    run
}

fn run_fixture_output_in_dir(root: &Path, name: &str, current_dir: &Path) -> Output {
    let output = compile_fixture(root, name);
    let run = runtime_test_runner(&output)
        .current_dir(current_dir)
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
    let root = repo_root().join("neco-felis");
    let temp_dir = runtime_temp_dir("neco-felis");
    let binary = compile_fixture(&root, "neco-felis");
    let input_path = repo_root().join("tests/testcases/exit-42/src/exit-42.fe");

    let run = runtime_test_runner(&binary)
        .current_dir(&temp_dir)
        .arg(&input_path)
        .output()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    let emitted = temp_dir.join("a.out");
    let emitted_bytes = fs::read(&emitted).expect("read emitted a.out");
    let emitted_run = runtime_test_runner(&emitted)
        .output()
        .unwrap_or_else(|error| panic!("run emitted a.out with qemu-x86_64: {error}"));

    fs::remove_file(&binary).expect("cleanup binary");
    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

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
    let root = repo_root().join("neco-felis");
    let temp_dir = runtime_temp_dir("neco-felis-hello-world");
    let binary = compile_fixture(&root, "neco-felis");
    let input_path = repo_root().join("tests/testcases/hello-world/src/hello-world.fe");

    let run = runtime_test_runner(&binary)
        .current_dir(&temp_dir)
        .arg(&input_path)
        .output()
        .unwrap_or_else(|error| panic!("run binary with qemu-x86_64: {error}"));
    let emitted = temp_dir.join("a.out");
    let emitted_bytes = fs::read(&emitted).expect("read emitted a.out");
    let emitted_run = runtime_test_runner(&emitted)
        .output()
        .unwrap_or_else(|error| panic!("run emitted a.out with qemu-x86_64: {error}"));

    fs::remove_file(&binary).expect("cleanup binary");
    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(0));
    assert_eq!(emitted_run.stdout, b"Hello, world!\n");
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
