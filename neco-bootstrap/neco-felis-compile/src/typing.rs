use neco_felis_elaboration::PhaseElaborated;
use neco_felis_syn::File;
use neco_felis_typing::{
    BuiltinTypes, IntegerType, Type, TypeChecker, TypeHole, TypingError, TypingResult,
};

pub fn builtin_types() -> BuiltinTypes {
    let u64 = Type::Integer(IntegerType::U64);
    let f32 = Type::F32;
    let binary_u64 = arrow_chain(vec![u64.clone(), u64.clone()], u64.clone());
    let binary_f32 = arrow_chain(vec![f32.clone(), f32.clone()], f32.clone());
    let syscall = arrow_chain(vec![u64.clone(); 6], u64.clone());
    let array_param = Type::Hole(TypeHole(0));
    let array_elem = Type::Struct(Vec::new());
    let array = Type::arrow(array_param, array_elem);

    BuiltinTypes::new([
        ("syscall", syscall),
        ("u64_add", binary_u64.clone()),
        ("u64_sub", binary_u64.clone()),
        ("u64_mul", binary_u64.clone()),
        ("u64_div", binary_u64.clone()),
        ("u64_mod", binary_u64.clone()),
        ("u64_eq", binary_u64.clone()),
        ("f32_add", binary_f32.clone()),
        ("f32_sub", binary_f32.clone()),
        ("f32_mul", binary_f32.clone()),
        ("f32_div", binary_f32.clone()),
        ("f32_sqrt_approx", arrow_chain(vec![f32.clone()], f32.clone())),
        ("u64_to_f32", arrow_chain(vec![u64.clone()], f32.clone())),
        ("f32_to_u64", arrow_chain(vec![f32.clone()], u64.clone())),
        ("u64", u64.clone()),
        ("f32", f32.clone()),
        ("ctaid_x", u64.clone()),
        ("ntid_x", u64.clone()),
        ("tid_x", u64.clone()),
        ("Array", array),
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
