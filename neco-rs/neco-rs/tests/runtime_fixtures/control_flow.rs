use super::support::{repo_root, run_fixture_status};

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
fn compiles_and_runs_if_bool_function_condition_fixture() {
    let root = repo_root().join("tests/testcases/if");
    let status = run_fixture_status(&root, "if-bool-function-condition");
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
