use super::support::{repo_root, run_neco_felis_fixture};

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
fn compiles_and_runs_exit_0_with_neco_felis_fixture() {
    let input_path = repo_root().join("tests/testcases/exit-0");
    let (run, emitted_bytes, emitted_run) =
        run_neco_felis_fixture(&input_path, "neco-felis-exit-0");

    assert_eq!(run.status.code(), Some(0));
    assert!(run.stdout.is_empty());
    assert!(run.stderr.is_empty());
    assert_eq!(&emitted_bytes[0..4], b"\x7FELF");
    assert_eq!(emitted_run.status.code(), Some(0));
    assert!(emitted_run.stdout.is_empty());
    assert!(emitted_run.stderr.is_empty());
}

#[test]
#[ignore]
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
#[ignore]
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
#[ignore]
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
