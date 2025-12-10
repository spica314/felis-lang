use neco_felis_elaboration::NameId;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeHole(pub usize);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Hole(TypeHole),
    Variable(NameId),
    Arrow {
        param: Box<Type>,
        param_name: Option<NameId>,
        result: Box<Type>,
    },
    Apply {
        function: Box<Type>,
        argument: Box<Type>,
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
    pub ty: Type,
}

impl Type {
    pub fn hole(id: TypeHole) -> Self {
        Type::Hole(id)
    }

    pub fn arrow(param: Type, result: Type) -> Self {
        Type::Arrow {
            param: Box::new(param),
            param_name: None,
            result: Box::new(result),
        }
    }

    pub fn from_number_literal(literal: &str) -> Self {
        if literal.ends_with("i8") {
            Type::Integer(IntegerType::I8)
        } else if literal.ends_with("i16") {
            Type::Integer(IntegerType::I16)
        } else if literal.ends_with("i32") {
            Type::Integer(IntegerType::I32)
        } else if literal.ends_with("i64") {
            Type::Integer(IntegerType::I64)
        } else if literal.ends_with("u8") {
            Type::Integer(IntegerType::U8)
        } else if literal.ends_with("u16") {
            Type::Integer(IntegerType::U16)
        } else if literal.ends_with("u32") {
            Type::Integer(IntegerType::U32)
        } else if literal.ends_with("u64") {
            Type::Integer(IntegerType::U64)
        } else if literal.ends_with("f32") || literal.contains('.') {
            Type::F32
        } else {
            Type::Integer(IntegerType::U64)
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Hole(TypeHole(id)) => write!(f, "_{id}"),
            Type::Variable(name_id) => write!(f, "var#{name_id:?}"),
            Type::Arrow {
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
            Type::Apply { function, argument } => write!(f, "{function} {argument}"),
            Type::Struct(fields) => {
                write!(f, "struct {{ ")?;
                for (idx, field) in fields.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", field.name, field.ty)?;
                }
                write!(f, " }}")
            }
            Type::Unit => write!(f, "()"),
            Type::Integer(int) => write!(f, "{int}"),
            Type::F32 => write!(f, "f32"),
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
