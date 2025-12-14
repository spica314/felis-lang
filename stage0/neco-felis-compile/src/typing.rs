use neco_felis_elaboration::PhaseElaborated;
use neco_felis_syn::File;
use neco_felis_typing::{
    BuiltinTypes, IntegerType, Term, TypeChecker, TypeHole, TypingError, TypingResult,
};

pub fn builtin_types() -> BuiltinTypes {
    let u64 = Term::Integer(IntegerType::U64);
    let f32 = Term::F32;
    let binary_u64 = arrow_chain(vec![u64.clone(), u64.clone()], u64.clone());
    let binary_f32 = arrow_chain(vec![f32.clone(), f32.clone()], f32.clone());
    let syscall = arrow_chain(vec![u64.clone(); 6], u64.clone());
    let array_param = Term::Hole(TypeHole(0));
    let array_elem = Term::Struct(Vec::new());
    let array = Term::arrow(array_param.clone(), array_elem);
    let array_new_with_size = arrow_chain(
        vec![array_param.clone(), u64.clone()],
        Term::Hole(TypeHole(1)),
    );
    let array_get = arrow_chain(vec![array.clone(), u64.clone()], array_param.clone());
    let array_set = arrow_chain(
        vec![array.clone(), u64.clone(), array_param.clone()],
        array_param.clone(),
    );
    let array_len = arrow_chain(vec![array.clone()], u64.clone());
    let deref = arrow_chain(vec![array_param.clone()], array_param.clone());

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
        (
            "f32_sqrt_approx",
            arrow_chain(vec![f32.clone()], f32.clone()),
        ),
        ("u64_to_f32", arrow_chain(vec![u64.clone()], f32.clone())),
        ("f32_to_u64", arrow_chain(vec![f32.clone()], u64.clone())),
        ("u64", u64.clone()),
        ("f32", f32.clone()),
        ("Array", array),
        ("array_new_with_size", array_new_with_size),
        ("array_get", array_get),
        ("array_set", array_set),
        ("array_len", array_len),
        ("deref", deref),
    ])
}

pub fn check_types(file: &File<PhaseElaborated>) -> Result<TypingResult, TypingError> {
    TypeChecker::new(builtin_types()).check_file(file)
}

fn arrow_chain(params: Vec<Term>, result: Term) -> Term {
    params
        .into_iter()
        .rfold(result, |acc, param| Term::arrow(param, acc))
}
