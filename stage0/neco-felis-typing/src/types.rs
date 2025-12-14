use neco_felis_elaboration::NameId;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeHole(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Term {
    Hole(TypeHole),
    Variable(NameId),
    Arrow {
        param: Box<Term>,
        param_name: Option<NameId>,
        result: Box<Term>,
    },
    Apply {
        function: Box<Term>,
        argument: Box<Term>,
    },
    Struct(Vec<StructFieldType>),
    Unit,
    Integer(IntegerType),
    F32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntegerType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructFieldType {
    pub name: String,
    pub ty: Term,
}

impl Term {
    pub fn hole(id: TypeHole) -> Self {
        Term::Hole(id)
    }

    pub fn arrow(param: Term, result: Term) -> Self {
        Term::Arrow {
            param: Box::new(param),
            param_name: None,
            result: Box::new(result),
        }
    }

    pub fn from_number_literal(literal: &str) -> Self {
        if literal.ends_with("i8") {
            Term::Integer(IntegerType::I8)
        } else if literal.ends_with("i16") {
            Term::Integer(IntegerType::I16)
        } else if literal.ends_with("i32") {
            Term::Integer(IntegerType::I32)
        } else if literal.ends_with("i64") {
            Term::Integer(IntegerType::I64)
        } else if literal.ends_with("u8") {
            Term::Integer(IntegerType::U8)
        } else if literal.ends_with("u16") {
            Term::Integer(IntegerType::U16)
        } else if literal.ends_with("u32") {
            Term::Integer(IntegerType::U32)
        } else if literal.ends_with("u64") {
            Term::Integer(IntegerType::U64)
        } else if literal.ends_with("f32") || literal.contains('.') {
            Term::F32
        } else {
            Term::Integer(IntegerType::U64)
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Hole(TypeHole(id)) => write!(f, "_{id}"),
            Term::Variable(name_id) => write!(f, "var#{name_id:?}"),
            Term::Arrow {
                param,
                param_name,
                result,
            } => {
                if let Some(name) = param_name {
                    write!(f, "({name:?} : {param}) -> {result}")
                } else {
                    write!(f, "({param}) -> {result}")
                }
            }
            Term::Apply { function, argument } => write!(f, "{function} {argument}"),
            Term::Struct(fields) => {
                write!(f, "struct {{ ")?;
                for (idx, field) in fields.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.ty)?;
                }
                write!(f, " }}")
            }
            Term::Unit => write!(f, "()"),
            Term::Integer(int) => write!(f, "{int}"),
            Term::F32 => write!(f, "f32"),
        }
    }
}

impl fmt::Display for IntegerType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IntegerType::I8 => write!(f, "i8"),
            IntegerType::I16 => write!(f, "i16"),
            IntegerType::I32 => write!(f, "i32"),
            IntegerType::I64 => write!(f, "i64"),
            IntegerType::U8 => write!(f, "u8"),
            IntegerType::U16 => write!(f, "u16"),
            IntegerType::U32 => write!(f, "u32"),
            IntegerType::U64 => write!(f, "u64"),
        }
    }
}
