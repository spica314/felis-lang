use crate::Type;
use std::collections::HashMap;

/// Mapping from builtin name (string in `#use_builtin "<name>"`) to its type.
#[derive(Debug, Clone, Default)]
pub struct BuiltinTypes {
    types: HashMap<String, Type>,
}

impl BuiltinTypes {
    pub fn new(types: impl IntoIterator<Item = (impl Into<String>, Type)>) -> Self {
        let mut map = HashMap::new();
        for (name, ty) in types {
            map.insert(name.into(), ty);
        }
        Self { types: map }
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.types.get(name)
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    /// Return the highest type-hole id that appears in any builtin type, if any.
    pub fn max_type_hole_id(&self) -> Option<usize> {
        fn max_in_type(ty: &Type) -> Option<usize> {
            match ty {
                Type::Hole(crate::TypeHole(id)) => Some(*id),
                Type::Variable(_) | Type::Unit | Type::Integer(_) | Type::F32 => None,
                Type::Arrow { param, result, .. } => max_in_iter([param.as_ref(), result.as_ref()]),
                Type::Apply { function, argument } => {
                    max_in_iter([function.as_ref(), argument.as_ref()])
                }
                Type::Struct(fields) => fields.iter().filter_map(|f| max_in_type(&f.ty)).max(),
            }
        }

        fn max_in_iter<'a>(types: impl IntoIterator<Item = &'a Type>) -> Option<usize> {
            types.into_iter().filter_map(max_in_type).max()
        }

        self.types.values().filter_map(max_in_type).max()
    }
}
