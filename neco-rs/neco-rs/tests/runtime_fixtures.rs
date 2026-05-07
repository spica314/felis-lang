#[path = "runtime_fixtures/control_flow.rs"]
mod control_flow;
#[path = "runtime_fixtures/io.rs"]
mod io;
#[path = "runtime_fixtures/neco_felis.rs"]
mod neco_felis;
#[path = "runtime_fixtures/support.rs"]
mod support;

use support::*;

#[test]
fn compiles_and_runs_i32_ops_fixture() {
    let root = repo_root().join("tests/testcases/i32-ops");
    let status = run_fixture_status(&root, "i32-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_f32_ops_fixture() {
    let root = repo_root().join("tests/testcases/f32-ops");
    let status = run_fixture_status(&root, "f32-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_function_generics_fixture() {
    let root = repo_root().join("tests/testcases/function-generics");
    let status = run_fixture_status(&root, "function-generics");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_dyn_array_generic_get_fixture() {
    let root = repo_root().join("tests/testcases/dyn-array-generic-get");
    let status = run_fixture_status(&root, "dyn-array-generic-get");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_dyn_array_generic_push_fixture() {
    let root = repo_root().join("tests/testcases/dyn-array-generic-push");
    let status = run_fixture_status(&root, "dyn-array-generic-push");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_generics_some_fixture() {
    let root = repo_root().join("tests/testcases/type-generics");
    let status = run_fixture_status(&root, "type-generics-some");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_generics_none_fixture() {
    let root = repo_root().join("tests/testcases/type-generics");
    let status = run_fixture_status(&root, "type-generics-none");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_generics_ok_fixture() {
    let root = repo_root().join("tests/testcases/type-generics");
    let status = run_fixture_status(&root, "type-generics-ok");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_generics_err_fixture() {
    let root = repo_root().join("tests/testcases/type-generics");
    let status = run_fixture_status(&root, "type-generics-err");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_std_option_some_fixture() {
    let root = repo_root().join("tests/testcases/std-option-result");
    let status = run_fixture_status(&root, "std-option-some");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_std_option_none_fixture() {
    let root = repo_root().join("tests/testcases/std-option-result");
    let status = run_fixture_status(&root, "std-option-none");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_std_result_ok_fixture() {
    let root = repo_root().join("tests/testcases/std-option-result");
    let status = run_fixture_status(&root, "std-result-ok");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_std_result_err_fixture() {
    let root = repo_root().join("tests/testcases/std-option-result");
    let status = run_fixture_status(&root, "std-result-err");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_i64_ops_fixture() {
    let root = repo_root().join("tests/testcases/i64-ops");
    let status = run_fixture_status(&root, "i64-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_negative_i64_literal_fixture() {
    let root = repo_root().join("tests/testcases/negative-i64-literal");
    let status = run_fixture_status(&root, "negative-i64-literal");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_i64_reference_annotation_fixture() {
    let root = repo_root().join("tests/testcases/i64-reference-annotation");
    let status = run_fixture_status(&root, "i64-reference-annotation");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_i64_array_fixture() {
    let root = repo_root().join("tests/testcases/i64-array");
    let status = run_fixture_status(&root, "i64-array");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_u8_ops_fixture() {
    let root = repo_root().join("tests/testcases/u8-ops");
    let status = run_fixture_status(&root, "u8-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_primitive_conversions_fixture() {
    let root = repo_root().join("tests/testcases/primitive-conversions");
    let status = run_fixture_status(&root, "primitive-conversions");
    assert_eq!(status.code(), Some(112));
}

#[test]
fn compiles_and_runs_bool_basic_fixture() {
    let root = repo_root().join("tests/testcases/bool-basic");
    let status = run_fixture_status(&root, "bool-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_exit_0_fixture() {
    let root = repo_root().join("tests/testcases/exit-0");
    let status = run_fixture_status(&root, "exit-0");
    assert_eq!(status.code(), Some(0));
}

#[test]
fn compiles_and_runs_nested_user_call_argument_fixture() {
    let root = repo_root().join("tests/testcases/compiler-expression-limits");
    let status = run_fixture_status(&root, "nested-user-call-argument");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_nested_conversion_argument_fixture() {
    let root = repo_root().join("tests/testcases/compiler-expression-limits");
    let status = run_fixture_status(&root, "nested-conversion-argument");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_equality_true_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "enum-eq-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_equality_false_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "enum-eq-false");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_payload_equality_true_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "enum-payload-eq-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_enum_payload_equality_false_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "enum-payload-eq-false");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_equality_true_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "struct-eq-true");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_equality_false_fixture() {
    let root = repo_root().join("tests/testcases/equality-basic");
    let status = run_fixture_status(&root, "struct-eq-false");
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
fn compiles_and_runs_arrayvl_len_fixture() {
    let root = repo_root().join("tests/testcases/arrayvl-len");
    let status = run_fixture_status(&root, "arrayvl-len");
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
fn compiles_and_runs_reference_builtins_fixture() {
    let root = repo_root().join("tests/testcases/reference-builtins");
    let status = run_fixture_status(&root, "reference-builtins");
    assert_eq!(status.code(), Some(82));
}

#[test]
fn rejects_shared_primitive_ref_set_fixture() {
    let root = repo_root().join("tests/testcases/primitive-reference-mutability");
    let error = compile_fixture_error(&root, "shared-ref-set");
    assert!(error.contains("`ref_set` requires an exclusive reference"));
}

#[test]
fn rejects_shared_primitive_reference_exclusive_parameter_fixture() {
    let root = repo_root().join("tests/testcases/primitive-reference-mutability");
    let error = compile_fixture_error(&root, "shared-ref-exclusive-parameter");
    assert!(error.contains("expected a value of type `&^ i32`"));
}

#[test]
fn compiles_and_runs_fn_reference_annotation_fixture() {
    let root = repo_root().join("tests/testcases/fn-reference-annotation");
    let status = run_fixture_status(&root, "fn-reference-annotation");
    assert_eq!(status.code(), Some(43));
}

#[test]
fn compiles_and_runs_fn_return_fixture() {
    let root = repo_root().join("tests/testcases/fn-return");
    let status = run_fixture_status(&root, "fn-return");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_fn_cli_arg_reference_fixture() {
    let root = repo_root().join("tests/testcases/fn-cli-arg-reference");
    let (output, mut child) = compile_and_spawn_fixture(&root, "fn-cli-arg-reference", |command| {
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
fn compiles_and_runs_signed_hex_literals_fixture() {
    let root = repo_root().join("tests/testcases/signed-hex-literals");
    let status = run_fixture_status(&root, "signed-hex-literals");
    assert_eq!(status.code(), Some(42));
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
fn compiles_and_runs_struct_nested_field_access_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-nested-field-access");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_rc_field_access_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-rc-field-access");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_reference_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_mut_reference_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-mut-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_rc_reference_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-rc-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_rc_mut_reference_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-rc-mut-reference");
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
fn compiles_and_runs_enum_match_string_fixture() {
    let root = repo_root().join("tests/testcases/enum-match-string");
    let status = run_fixture_status(&root, "enum-match-string");
    assert_eq!(status.code(), Some(65));
}

#[test]
fn compiles_and_runs_enum_match_struct_string_fixture() {
    let root = repo_root().join("tests/testcases/enum-match-struct-string");
    let status = run_fixture_status(&root, "enum-match-struct-string");
    assert_eq!(status.code(), Some(65));
}

#[test]
fn compiles_and_runs_type_rc_match_single_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-single");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_match_reference_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-match-reference");
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
fn compiles_and_runs_type_rc_match_reference_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_mut_reference_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-mut-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_mut_rc_reference_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-mut-rc-reference");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_match_fn_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-match");
    let status = run_fixture_status(&root, "type-rc-match-fn");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_type_rc_parser_basic_fixture() {
    let root = repo_root().join("tests/testcases/type-rc-parser-basic");
    let status = run_fixture_status(&root, "type-rc-parser-basic");
    assert_eq!(status.code(), Some(42));
}

#[test]
#[ignore]
fn compiles_and_runs_neco_felis_parser_string_expr_fixture() {
    let root = repo_root().join("neco-felis/neco-felis-parser");
    let status = run_fixture_status(&root, "felis-parser-string-expr");
    assert_eq!(status.code(), Some(5));
}

#[test]
#[ignore]
fn compiles_and_runs_neco_felis_lexer_api_fixture() {
    let root = repo_root().join("neco-felis/neco-felis-parser");
    let status = run_fixture_status(&root, "felis-lexer-api");
    assert_eq!(status.code(), Some(42));
}
