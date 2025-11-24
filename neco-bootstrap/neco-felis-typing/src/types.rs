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
    Number,
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
            Type::Number => write!(f, "Number"),
        }
    }
}
