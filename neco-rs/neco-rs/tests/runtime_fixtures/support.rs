use neco_rs::compile_path_to_elf;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

static NECO_FELIS_BINARY: OnceLock<PathBuf> = OnceLock::new();

pub(super) fn repo_root() -> PathBuf {
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

fn neco_felis_binary() -> &'static Path {
    NECO_FELIS_BINARY
        .get_or_init(|| {
            let root = repo_root().join("neco-felis");
            compile_fixture(&root, "neco-felis")
        })
        .as_path()
}

pub(super) fn runtime_temp_dir(name: &str) -> PathBuf {
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

pub(super) fn compile_and_spawn_fixture(
    root: &Path,
    name: &str,
    configure: impl FnOnce(&mut Command),
) -> (PathBuf, Child) {
    let output = compile_fixture(root, name);
    let mut command = runtime_test_runner(&output);
    configure(&mut command);
    let child = spawn_runtime_command(&mut command, "run fixture binary");
    (output, child)
}

fn spawn_runtime_command(command: &mut Command, context: &str) -> Child {
    const TEXT_FILE_BUSY: i32 = 26;
    const MAX_ATTEMPTS: usize = 20;

    for attempt in 1..=MAX_ATTEMPTS {
        match command.spawn() {
            Ok(child) => return child,
            Err(error)
                if error.raw_os_error() == Some(TEXT_FILE_BUSY) && attempt < MAX_ATTEMPTS =>
            {
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(error) => panic!("{context}: {error}"),
        }
    }

    unreachable!("spawn loop always returns or panics")
}

pub(super) fn cleanup_fixture_binary(output: &Path) {
    fs::remove_file(output).expect("cleanup binary");
    fs::remove_dir(output.parent().expect("build temp dir")).expect("cleanup build temp dir");
}

pub(super) fn run_fixture_status(root: &Path, name: &str) -> std::process::ExitStatus {
    let (output, mut child) = compile_and_spawn_fixture(root, name, |_| {});
    let status = child.wait().expect("collect child status");
    cleanup_fixture_binary(&output);
    status
}

pub(super) fn run_fixture_output(root: &Path, name: &str) -> Output {
    let (output, child) = compile_and_spawn_fixture(root, name, |command| {
        command.stdout(Stdio::piped()).stderr(Stdio::piped());
    });
    let run = child.wait_with_output().expect("collect child output");
    cleanup_fixture_binary(&output);
    run
}

pub(super) fn run_fixture_output_with_args(root: &Path, name: &str, args: &[&str]) -> Output {
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

pub(super) fn run_fixture_output_in_dir(root: &Path, name: &str, current_dir: &Path) -> Output {
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

pub(super) fn run_fixture_with_input(root: &Path, name: &str, stdin: &[u8]) -> Output {
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

pub(super) fn run_neco_felis_fixture(
    input_root: &Path,
    temp_name: &str,
) -> (Output, Vec<u8>, Output) {
    let temp_dir = runtime_temp_dir(temp_name);
    let fixture_name = input_root
        .file_name()
        .and_then(|name| name.to_str())
        .expect("fixture input root name");
    let source = fs::read(input_root.join("src").join(format!("{fixture_name}.fe")))
        .expect("read neco-felis fixture source");

    let binary = neco_felis_binary();
    let mut command = runtime_test_runner(binary);
    command
        .current_dir(&temp_dir)
        .arg(input_root)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = spawn_runtime_command(&mut command, "run neco-felis fixture binary");
    child
        .stdin
        .as_mut()
        .expect("child stdin")
        .write_all(&source)
        .expect("write neco-felis fixture source");
    let run = child
        .wait_with_output()
        .expect("collect neco-felis fixture output");
    let emitted = temp_dir.join("a.out");
    let emitted_bytes = fs::read(&emitted).expect("read emitted a.out");
    let mut command = runtime_test_runner(&emitted);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let emitted_child = spawn_runtime_command(&mut command, "run emitted a.out");
    let emitted_run = emitted_child
        .wait_with_output()
        .expect("collect emitted a.out output");

    fs::remove_dir_all(&temp_dir).expect("cleanup runtime temp dir");

    (run, emitted_bytes, emitted_run)
}
