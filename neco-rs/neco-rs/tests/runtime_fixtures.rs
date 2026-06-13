#[path = "runtime_fixtures/control_flow.rs"]
mod control_flow;
#[path = "runtime_fixtures/io.rs"]
mod io;
#[path = "runtime_fixtures/neco_felis.rs"]
mod neco_felis;
#[path = "runtime_fixtures/support.rs"]
mod support;

use std::fs;
use std::process::Command;
use support::*;

#[test]
fn compiles_and_runs_i32_ops_fixture() {
    let root = repo_root().join("tests/testcases/i32-ops");
    let status = run_fixture_status(&root, "i32-ops");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_libc_start_fixture() {
    let root = repo_root().join("tests/testcases/libc-start-hello");
    let output = run_fixture_output(&root, "libc-start-hello");
    assert!(output.status.success());
    assert_eq!(output.stdout, b"Hello from libc start\n");
}

#[test]
fn compiles_and_runs_panic_basic_fixture() {
    let root = repo_root().join("tests/testcases/panic-basic");
    let output = run_fixture_output(&root, "panic-basic");
    assert_eq!(output.status.code(), Some(101));
    assert!(output.stdout.is_empty());
    assert_eq!(output.stderr, b"panic-basic\n");
}

#[test]
fn libc_start_fixture_links_requested_shared_libraries() {
    let root = repo_root().join("tests/testcases/libc-start-hello");
    let output = compile_fixture(&root, "libc-start-hello");
    let dynamic = Command::new("readelf")
        .arg("-d")
        .arg(&output)
        .output()
        .expect("run readelf");
    cleanup_fixture_binary(&output);

    assert!(dynamic.status.success());
    let dynamic = String::from_utf8_lossy(&dynamic.stdout);
    assert!(dynamic.contains("Shared library: [libc.so.6]"));
    assert!(dynamic.contains("Shared library: [libm.so.6]"));
}

#[test]
fn libc_start_fixture_uses_crt_start_as_elf_entry() {
    let root = repo_root().join("tests/testcases/libc-start-hello");
    let output = compile_fixture(&root, "libc-start-hello");
    let headers_and_symbols = Command::new("readelf")
        .arg("-h")
        .arg("-s")
        .arg(&output)
        .output()
        .expect("run readelf");
    cleanup_fixture_binary(&output);

    assert!(headers_and_symbols.status.success());
    let headers_and_symbols = String::from_utf8_lossy(&headers_and_symbols.stdout);
    let entry = readelf_entry_address(&headers_and_symbols);
    let start = readelf_symbol_address(&headers_and_symbols, "_start");
    let main = readelf_symbol_address(&headers_and_symbols, "main");
    assert_eq!(entry, start);
    assert_ne!(entry, main);
}

#[test]
fn selected_binary_ignores_unselected_private_modules() {
    let root = repo_root().join("tests/testcases/multi-bin-private-modules");

    let server = run_fixture_status(&root, "server");
    assert_eq!(server.code(), Some(42));

    let tool = run_fixture_status(&root, "tool");
    assert_eq!(tool.code(), Some(7));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_cu_init_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-cu-init");
    let status = run_fixture_status(&root, "cuda-cu-init");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_cu_device_ctx_create_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-cu-device-ctx-create");
    let status = run_fixture_status(&root, "cuda-cu-device-ctx-create");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_module_load_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-module-load");
    let status = run_fixture_status(&root, "cuda-compile-ptx-module-load");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_arithmetic_module_load_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-arithmetic-module-load");
    let status = run_fixture_status(&root, "cuda-compile-ptx-arithmetic-module-load");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_f32_launch_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-f32-launch");
    let status = run_fixture_status(&root, "cuda-compile-ptx-f32-launch");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_render_launch_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-render-launch");
    let status = run_fixture_status(&root, "cuda-compile-ptx-render-launch");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_struct_module_load_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-struct-module-load");
    let status = run_fixture_status(&root, "cuda-compile-ptx-struct-module-load");
    assert_eq!(status.code(), Some(0));
}

#[test]
#[ignore = "requires a CUDA driver installation with libcuda.so available to the native linker"]
fn compiles_and_runs_cuda_compile_ptx_special_registers_module_load_fixture() {
    if std::env::var_os("NECO_RS_TEST_CUDA").is_none() {
        return;
    }

    let root = repo_root().join("tests/testcases/cuda-compile-ptx-special-registers-module-load");
    let status = run_fixture_status(&root, "cuda-compile-ptx-special-registers-module-load");
    assert_eq!(status.code(), Some(0));
}

#[test]
fn compiles_and_runs_compile_ptx_function_call_fixture() {
    let root = repo_root().join("tests/testcases/compile-ptx-function-call");
    let status = run_fixture_status(&root, "compile-ptx-function-call");
    assert_eq!(status.code(), Some(0));
}

#[test]
fn compiles_and_runs_neco_sat_test_binaries() {
    let root = repo_root().join("projects/neco-sat");

    let sat = run_fixture_status(&root, "neco-sat-test-sat");
    assert_eq!(sat.code(), Some(0));

    let unsat = run_fixture_status(&root, "neco-sat-test-unsat");
    assert_eq!(unsat.code(), Some(0));

    let xor_associative = run_fixture_status(&root, "neco-sat-test-xor-associative");
    assert_eq!(xor_associative.code(), Some(0));
}

#[test]
fn compiles_and_runs_neco_sat_main_with_dimacs_file_input() {
    let root = repo_root().join("projects/neco-sat");
    let input = fs::read(root.join("fixtures/sat.cnf")).expect("read DIMACS fixture");

    let output = run_fixture_with_input(&root, "neco-sat-main", &input);
    assert_eq!(output.status.code(), Some(10));
    assert_eq!(output.stdout, b"SAT\n");
    assert!(output.stderr.is_empty());
}

fn readelf_entry_address(output: &str) -> u64 {
    output
        .lines()
        .find_map(|line| {
            line.trim()
                .strip_prefix("Entry point address:")
                .and_then(|value| {
                    u64::from_str_radix(value.trim().trim_start_matches("0x"), 16).ok()
                })
        })
        .expect("readelf entry address")
}

fn readelf_symbol_address(output: &str, symbol: &str) -> u64 {
    output
        .lines()
        .find_map(|line| {
            let parts = line.split_whitespace().collect::<Vec<_>>();
            (parts.last() == Some(&symbol))
                .then(|| u64::from_str_radix(parts.get(1).copied().unwrap_or_default(), 16).ok())
                .flatten()
        })
        .expect("readelf symbol address")
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
fn compiles_and_runs_u8_reference_set_fixture() {
    let root = repo_root().join("tests/testcases/primitive-reference-values");
    let status = run_fixture_status(&root, "u8-ref-set");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_bool_reference_set_fixture() {
    let root = repo_root().join("tests/testcases/primitive-reference-values");
    let status = run_fixture_status(&root, "bool-ref-set");
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
fn rejects_struct_ref_set_replacement_type_mismatch_fixture() {
    let root = repo_root().join("tests/testcases/ref-set-validation");
    let error = compile_fixture_error(&root, "struct-replacement-type-mismatch");
    assert!(error.contains("expected a value of type `Span`"));
}

#[test]
fn rejects_constructor_ref_set_replacement_type_mismatch_fixture() {
    let root = repo_root().join("tests/testcases/ref-set-validation");
    let error = compile_fixture_error(&root, "constructor-replacement-type-mismatch");
    assert!(error.contains("expected a value of type `LocalOption`"));
}

#[test]
fn rejects_shared_primitive_reference_exclusive_parameter_fixture() {
    let root = repo_root().join("tests/testcases/primitive-reference-mutability");
    let error = compile_fixture_error(&root, "shared-ref-exclusive-parameter");
    assert!(error.contains("expected a value of type `&^ i32`"));
}

#[test]
fn rejects_entrypoint_parameter_fixture() {
    let root = repo_root().join("tests/testcases/entrypoint-validation");
    let error = compile_fixture_error(&root, "entrypoint-parameter");
    assert!(error.contains("entrypoint `main` must not declare parameters"));
}

#[test]
fn rejects_entrypoint_non_unit_result_fixture() {
    let root = repo_root().join("tests/testcases/entrypoint-validation");
    let error = compile_fixture_error(&root, "entrypoint-non-unit-result");
    assert!(error.contains("entrypoint `main` must declare result type `()`"));
}

#[test]
fn rejects_multiple_entrypoints_fixture() {
    let root = repo_root().join("tests/testcases/entrypoint-validation");
    let error = compile_fixture_error(&root, "multiple-entrypoints");
    assert!(error.contains("multiple #entrypoint declarations are not supported: main, fallback"));
}

#[test]
fn rejects_unknown_type_modifier_fixture() {
    let root = repo_root().join("tests/testcases/modifier-validation");
    let error = compile_fixture_error(&root, "unknown-type-modifier");
    assert!(error.contains("unknown type modifier `refcounted` on `Token`"));
}

#[test]
fn rejects_unknown_struct_modifier_fixture() {
    let root = repo_root().join("tests/testcases/modifier-validation");
    let error = compile_fixture_error(&root, "unknown-struct-modifier");
    assert!(error.contains("unknown struct modifier `refcounted` on `Point`"));
}

#[test]
fn rejects_unknown_function_effect_fixture() {
    let root = repo_root().join("tests/testcases/effect-validation");
    let error = compile_fixture_error(&root, "unknown-function-effect");
    assert!(error.contains("function `helper` effect must be `IO`"));
}

#[test]
fn rejects_effectful_call_bound_with_equals_fixture() {
    let root = repo_root().join("tests/testcases/effect-validation");
    let error = compile_fixture_error(&root, "effectful-call-equals");
    assert!(error.contains("`read_status` is effectful and must be bound with `<-`"));
}

#[test]
fn rejects_effectful_reference_helper_bound_with_equals_fixture() {
    let root = repo_root().join("tests/testcases/effect-validation");
    let error = compile_fixture_error(&root, "effectful-reference-helper-equals");
    assert!(error.contains("`add_into_ref` is effectful and must be bound with `<-`"));
}

#[test]
fn rejects_effectful_array_helper_bound_with_equals_fixture() {
    let root = repo_root().join("tests/testcases/effect-validation");
    let error = compile_fixture_error(&root, "effectful-array-helper-equals");
    assert!(error.contains("`read_first` is effectful and must be bound with `<-`"));
}

#[test]
fn rejects_invalid_struct_kind_fixture() {
    let root = repo_root().join("tests/testcases/type-kind-validation");
    let error = compile_fixture_error(&root, "invalid-struct-kind");
    assert!(error.contains("struct `Point` kind must end in `Type[0]`"));
}

#[test]
fn rejects_invalid_type_kind_fixture() {
    let root = repo_root().join("tests/testcases/type-kind-validation");
    let error = compile_fixture_error(&root, "invalid-type-kind");
    assert!(error.contains("type `Value` kind must end in `Type[0]`"));
}

#[test]
fn rejects_invalid_constructor_result_type_fixture() {
    let root = repo_root().join("tests/testcases/constructor-validation");
    let error = compile_fixture_error(&root, "invalid-constructor-result-type");
    assert!(error.contains("constructor `single` result type must be `Value`"));
}

#[test]
fn rejects_struct_and_type_name_collision_fixture() {
    let root = repo_root().join("tests/testcases/type-name-collision");
    let error = compile_fixture_error(&root, "type-name-collision");
    assert!(error.contains("type name `Token` is already used by an algebraic type"));
}

#[test]
fn rejects_duplicate_pure_function_parameters_fixture() {
    let root = repo_root().join("tests/testcases/parameter-validation");
    let error = compile_fixture_error(&root, "duplicate-pure-function-parameters");
    assert!(error.contains("duplicate parameter `value` in function `pick`"));
}

#[test]
fn rejects_duplicate_io_function_parameters_fixture() {
    let root = repo_root().join("tests/testcases/parameter-validation");
    let error = compile_fixture_error(&root, "duplicate-io-function-parameters");
    assert!(error.contains("duplicate parameter `value` in function `write_value`"));
}

#[test]
fn rejects_duplicate_constructor_parameters_fixture() {
    let root = repo_root().join("tests/testcases/parameter-validation");
    let error = compile_fixture_error(&root, "duplicate-constructor-parameters");
    assert!(error.contains("duplicate parameter `value` in constructor `pair`"));
}

#[test]
fn rejects_pure_function_local_let_type_mismatch_fixture() {
    let root = repo_root().join("tests/testcases/local-let-validation");
    let error = compile_fixture_error(&root, "pure-let-type-mismatch");
    assert!(error.contains("expected a value of type `bool`"));
}

#[test]
fn rejects_statement_after_break_fixture() {
    let root = repo_root().join("tests/testcases/loop-control-validation");
    let error = compile_fixture_error(&root, "statement-after-break");
    assert!(error.contains("statements after loop control are not supported"));
}

#[test]
fn rejects_statement_after_continue_fixture() {
    let root = repo_root().join("tests/testcases/loop-control-validation");
    let error = compile_fixture_error(&root, "statement-after-continue");
    assert!(error.contains("statements after loop control are not supported"));
}

#[test]
fn rejects_if_tail_expression_fixture() {
    let root = repo_root().join("tests/testcases/control-flow-tail-validation");
    let error = compile_fixture_error(&root, "if-tail-expression");
    assert!(error.contains("tail expressions in `#if` statement blocks are not supported"));
}

#[test]
fn rejects_else_tail_expression_fixture() {
    let root = repo_root().join("tests/testcases/control-flow-tail-validation");
    let error = compile_fixture_error(&root, "else-tail-expression");
    assert!(error.contains("tail expressions in `#else` statement blocks are not supported"));
}

#[test]
fn rejects_loop_tail_expression_fixture() {
    let root = repo_root().join("tests/testcases/control-flow-tail-validation");
    let error = compile_fixture_error(&root, "loop-tail-expression");
    assert!(error.contains("tail expressions in `#loop` statement blocks are not supported"));
}

#[test]
fn rejects_statement_after_exit_in_if_fixture() {
    let root = repo_root().join("tests/testcases/exit-control-validation");
    let error = compile_fixture_error(&root, "statement-after-exit-in-if");
    assert!(error.contains("statements after `IO::sys_exit` are not supported"));
}

#[test]
fn rejects_statement_after_exit_in_else_fixture() {
    let root = repo_root().join("tests/testcases/exit-control-validation");
    let error = compile_fixture_error(&root, "statement-after-exit-in-else");
    assert!(error.contains("statements after `IO::sys_exit` are not supported"));
}

#[test]
fn rejects_statement_after_exit_in_loop_fixture() {
    let root = repo_root().join("tests/testcases/exit-control-validation");
    let error = compile_fixture_error(&root, "statement-after-exit-in-loop");
    assert!(error.contains("statements after `IO::sys_exit` are not supported"));
}

#[test]
fn rejects_statement_after_exit_in_block_fixture() {
    let root = repo_root().join("tests/testcases/exit-control-validation");
    let error = compile_fixture_error(&root, "statement-after-exit-in-block");
    assert!(error.contains("statements after `IO::sys_exit` are not supported"));
}

#[test]
fn rejects_statement_after_exit_in_match_fixture() {
    let root = repo_root().join("tests/testcases/exit-control-validation");
    let error = compile_fixture_error(&root, "statement-after-exit-in-match");
    assert!(error.contains("statements after `IO::sys_exit` are not supported"));
}

#[test]
fn rejects_unknown_applied_type_annotation_fixture() {
    let root = repo_root().join("tests/testcases/type-application-validation");
    let error = compile_fixture_error(&root, "unknown-applied-type");
    assert!(error.contains("expected a value of type `Missing i32`"));
}

#[test]
fn rejects_qualified_i32_primitive_fixture() {
    let root = repo_root().join("tests/testcases/qualified-builtin-validation");
    let error = compile_fixture_error(&root, "qualified-i32-primitive");
    assert!(
        error.contains("`i32 primitive` builtin call must use a simple builtin path"),
        "got {error}"
    );
}

#[test]
fn rejects_qualified_bool_comparison_fixture() {
    let root = repo_root().join("tests/testcases/qualified-builtin-validation");
    let error = compile_fixture_error(&root, "qualified-bool-comparison");
    assert!(
        error.contains("unsupported pure expression in entrypoint body"),
        "got {error}"
    );
}

#[test]
fn rejects_qualified_array_type_fixture() {
    let root = repo_root().join("tests/testcases/qualified-builtin-validation");
    let error = compile_fixture_error(&root, "qualified-array-type");
    assert!(
        error.contains("unsupported type annotation `missing::Array i32 1 i32`"),
        "got {error}"
    );
}

#[test]
fn rejects_type_universe_variable_annotation_fixture() {
    let root = repo_root().join("tests/testcases/type-application-validation");
    let error = compile_fixture_error(&root, "type-universe-variable");
    assert!(error.contains("expected a value of type `Type level`"));
}

#[test]
fn compiles_and_runs_f32_function_reference_slot_fixture() {
    let root = repo_root().join("tests/testcases/f32-reference-slots");
    let status = run_fixture_status(&root, "f32-function-reference-slot");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_f32_block_reference_slot_fixture() {
    let root = repo_root().join("tests/testcases/f32-reference-slots");
    let status = run_fixture_status(&root, "f32-block-reference-slot");
    assert_eq!(status.code(), Some(42));
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
fn compiles_and_runs_struct_mut_reference_block_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-mut-reference-block");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_mut_reference_if_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-mut-reference-if");
    assert_eq!(status.code(), Some(42));
}

#[test]
fn compiles_and_runs_struct_mut_reference_loop_fixture() {
    let root = repo_root().join("tests/testcases/struct-basic");
    let status = run_fixture_status(&root, "struct-mut-reference-loop");
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
