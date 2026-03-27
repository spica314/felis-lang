fn main() {
    if let Err(error) = neco_rs::run_cli(std::env::args()) {
        eprintln!("{error}");
        std::process::exit(1);
    }
}
