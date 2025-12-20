use neco_felis_syn::{File, FileIdGenerator, Parse, PhaseParse, token::Token};
use std::path::Path;

fn parse_fixture(rel_path: &str) {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let path = manifest_dir.join("..").join("..").join(rel_path);
    let source = std::fs::read_to_string(&path).expect("read fixture");

    let mut file_id_generator = FileIdGenerator::new();
    let file_id = file_id_generator.generate_file_id();
    let tokens = Token::lex(&source, file_id);

    let mut i = 0;
    let _parsed = File::<PhaseParse>::parse(&tokens, &mut i)
        .expect("parse failed")
        .expect("no file parsed");

    assert_eq!(
        i,
        tokens.len(),
        "parser did not consume all tokens for {}",
        path.display()
    );
}

#[test]
fn parse_submodule_root() {
    parse_fixture("testcases/felis/packages/submodule/root.fe");
}

#[test]
fn parse_submodule_sub() {
    parse_fixture("testcases/felis/packages/submodule/sub.fe");
}
