use neco_felis_elaboration::{PhaseElaborated, TermVariableIds, elaborate_file};
use neco_felis_syn::{File, FileIdGenerator, Item, Parse, PhaseParse, Term, TermVariable, Token};
use neco_felis_typing::{BuiltinTypes, IntegerType, Type, TypeChecker};

fn parse_and_elaborate(src: &str) -> File<PhaseElaborated> {
    let mut file_id_gen = FileIdGenerator::new();
    let file_id = file_id_gen.generate_file_id();
    let tokens = Token::lex(src, file_id);
    let mut i = 0;
    let parsed = File::<PhaseParse>::parse(&tokens, &mut i)
        .expect("parse failed")
        .expect("no file parsed");
    elaborate_file(&parsed).expect("elaboration failed")
}

#[test]
fn infers_simple_arrow_and_body() {
    let file = parse_and_elaborate("#definition main: () -> () { () }");
    let Item::Definition(def) = &file.items[0] else {
        panic!("expected definition");
    };

    let arrow_id = match def.type_.as_ref() {
        Term::ArrowNodep(arrow) => arrow.ext.term_id.clone(),
        other => panic!("expected arrow type, got {other:?}"),
    };
    let body_id = match def.body.as_ref() {
        Term::Unit(unit) => unit.ext.term_id.clone(),
        other => panic!("expected unit body, got {other:?}"),
    };

    let result = TypeChecker::new(BuiltinTypes::default())
        .check_file(&file)
        .expect("typing failed");

    let expected_arrow = Type::Arrow {
        param: Box::new(Type::Unit),
        param_name: None,
        result: Box::new(Type::Unit),
    };
    assert_eq!(result.resolved_type(&arrow_id), Some(expected_arrow));
    assert_eq!(result.resolved_type(&body_id), Some(Type::Unit));
}

#[test]
fn infers_dependent_arrow_parameter() {
    let mut file = parse_and_elaborate("#definition id: (x: ()) -> () { x }");
    let Item::Definition(def) = &mut file.items[0] else {
        panic!("expected definition");
    };

    let (arrow_id, param_id) = match def.type_.as_ref() {
        Term::ArrowDep(arrow) => (arrow.ext.term_id.clone(), arrow.ext.param_id.clone()),
        other => panic!("expected dependent arrow, got {other:?}"),
    };
    let body_id = match def.body.as_ref() {
        Term::Variable(var) => var.ext.term_id.clone(),
        other => panic!("expected variable body, got {other:?}"),
    };
    // Elaboration inserts a placeholder for the body variable; align it with the parameter id.
    if let Term::Variable(var) = def.body.as_mut() {
        let term_id = var.ext.term_id.clone();
        let updated = TermVariable {
            variable: var.variable.clone(),
            ext: TermVariableIds {
                term_id,
                name_id: param_id.clone(),
            },
        };
        *def.body = Term::Variable(updated);
    }

    let result = TypeChecker::new(BuiltinTypes::default())
        .check_file(&file)
        .expect("typing failed");

    let expected_ty = Type::Arrow {
        param: Box::new(Type::Unit),
        param_name: Some(param_id.clone()),
        result: Box::new(Type::Unit),
    };
    assert_eq!(result.resolved_type(&arrow_id), Some(expected_ty));
    assert_eq!(result.resolved_type(&body_id), Some(Type::Unit));
}

#[test]
fn binds_builtin_types_from_outside() {
    let mut file = parse_and_elaborate(
        r#"
#use_builtin "u64" #as u64;
#definition forty_two: u64 { 42 }
"#,
    );
    let alias_id = match &file.items[0] {
        Item::UseBuiltin(use_builtin) => use_builtin.ext.clone(),
        _ => panic!("expected first item to be use_builtin"),
    };

    let Item::Definition(def) = &mut file.items[1] else {
        panic!("expected second item to be definition");
    };

    let type_id = match def.type_.as_ref() {
        Term::Variable(var) => var.ext.term_id.clone(),
        other => panic!("expected variable type, got {other:?}"),
    };
    let body_id = match def.body.as_ref() {
        Term::Number(num) => num.ext.term_id.clone(),
        other => panic!("expected numeric body, got {other:?}"),
    };

    // Align the type variable with the alias introduced by use_builtin.
    if let Term::Variable(var) = def.type_.as_mut() {
        let term_id = var.ext.term_id.clone();
        let updated = TermVariable {
            variable: var.variable.clone(),
            ext: TermVariableIds {
                term_id,
                name_id: alias_id,
            },
        };
        *def.type_ = Term::Variable(updated);
    }

    let builtins = BuiltinTypes::new([("u64", Type::Integer(IntegerType::U64))]);
    let result = TypeChecker::new(builtins)
        .check_file(&file)
        .expect("typing failed");

    assert_eq!(
        result.resolved_type(&type_id),
        Some(Type::Integer(IntegerType::U64))
    );
    assert_eq!(
        result.resolved_type(&body_id),
        Some(Type::Integer(IntegerType::U64))
    );
}

fn literal_body_type(src: &str) -> Type {
    let file = parse_and_elaborate(src);
    let Item::Definition(def) = &file.items[0] else {
        panic!("expected definition");
    };
    let body_id = match def.body.as_ref() {
        Term::Number(num) => num.ext.term_id.clone(),
        other => panic!("expected number body, got {other:?}"),
    };

    TypeChecker::new(BuiltinTypes::default())
        .check_file(&file)
        .expect("typing failed")
        .resolved_type(&body_id)
        .expect("body type missing")
}

#[test]
fn infers_integer_literal_default_to_u64() {
    let ty = literal_body_type("#definition main: () -> () { 42 }");
    assert_eq!(ty, Type::Integer(IntegerType::U64));
}

#[test]
fn infers_integer_literal_suffixes() {
    let i32_ty = literal_body_type("#definition main: () -> () { 1i32 }");
    let u8_ty = literal_body_type("#definition main: () -> () { 255u8 }");
    assert_eq!(i32_ty, Type::Integer(IntegerType::I32));
    assert_eq!(u8_ty, Type::Integer(IntegerType::U8));
}

#[test]
fn infers_float_literal() {
    let f32_ty = literal_body_type("#definition main: () -> () { 1.0f32 }");
    assert_eq!(f32_ty, Type::F32);
}
