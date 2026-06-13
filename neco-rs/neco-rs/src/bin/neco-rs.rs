fn main() {
    let code = match neco_rs::run_cli(std::env::args()) {
        Ok(code) => code,
        Err(error) => {
            eprintln!("{error}");
            1
        }
    };
    std::process::exit(code);
}
