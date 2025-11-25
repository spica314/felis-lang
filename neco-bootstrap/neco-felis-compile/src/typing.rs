use neco_felis_elaboration::PhaseElaborated;
use neco_felis_syn::File;
use neco_felis_typing::{BuiltinTypes, Type, TypeChecker, TypingError, TypingResult};

pub fn builtin_types() -> BuiltinTypes {
    let number = Type::Number;
    let unary_number = arrow_chain(vec![number.clone()], number.clone());
    let binary_number = arrow_chain(vec![number.clone(), number.clone()], number.clone());
    let syscall = arrow_chain(vec![number.clone(); 6], number.clone());

    BuiltinTypes::new([
        ("syscall", syscall),
        ("u64_add", binary_number.clone()),
        ("u64_sub", binary_number.clone()),
        ("u64_mul", binary_number.clone()),
        ("u64_div", binary_number.clone()),
        ("u64_mod", binary_number.clone()),
        ("u64_eq", binary_number.clone()),
        ("f32_add", binary_number.clone()),
        ("f32_sub", binary_number.clone()),
        ("f32_mul", binary_number.clone()),
        ("f32_div", binary_number.clone()),
        ("f32_sqrt_approx", unary_number.clone()),
        ("u64_to_f32", unary_number.clone()),
        ("f32_to_u64", unary_number.clone()),
        ("u64", unary_number.clone()),
        ("f32", unary_number.clone()),
        ("ctaid_x", number.clone()),
        ("ntid_x", number.clone()),
        ("tid_x", number.clone()),
        ("Array", Type::Struct(Vec::new())),
    ])
}

pub fn check_types(file: &File<PhaseElaborated>) -> Result<TypingResult, TypingError> {
    TypeChecker::new(builtin_types()).check_file(file)
}

fn arrow_chain(params: Vec<Type>, result: Type) -> Type {
    params
        .into_iter()
        .rfold(result, |acc, param| Type::arrow(param, acc))
}
