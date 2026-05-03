use super::support::{
    repo_root, run_fixture_output_in_dir, run_fixture_output_with_args, run_fixture_with_input,
    runtime_temp_dir,
};
use std::fs;

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
fn compiles_and_runs_pathbuf_pop_fixture() {
    let root = repo_root().join("tests/testcases/pathbuf-pop");
    let run = run_fixture_output_in_dir(&root, "pathbuf-pop", &root);
    assert_eq!(run.status.code(), Some(0));
    assert_eq!(run.stdout, b"pathbuf/pop fixture\n");
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
