use crate::Term;
use std::collections::HashMap;

/// Mapping from builtin name (string in `#use_builtin "<name>"`) to its type.
#[derive(Debug, Clone, Default)]
pub struct BuiltinTypes {
    types: HashMap<String, Term>,
}

impl BuiltinTypes {
    pub fn new(types: impl IntoIterator<Item = (impl Into<String>, Term)>) -> Self {
        let mut map = HashMap::new();
        for (name, ty) in types {
            map.insert(name.into(), ty);
        }
        Self { types: map }
    }

    pub fn get(&self, name: &str) -> Option<&Term> {
        self.types.get(name)
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    /// Return the highest type-hole id that appears in any builtin type, if any.
    pub fn max_type_hole_id(&self) -> Option<usize> {
        fn max_in_type(ty: &Term) -> Option<usize> {
            match ty {
                Term::Hole(crate::TypeHole(id)) => Some(*id),
                Term::Variable(_) | Term::Unit | Term::Integer(_) | Term::F32 => None,
                Term::Arrow { param, result, .. } => max_in_iter([param.as_ref(), result.as_ref()]),
                Term::Apply { function, argument } => {
                    max_in_iter([function.as_ref(), argument.as_ref()])
                }
                Term::Struct(fields) => fields.iter().filter_map(|f| max_in_type(&f.ty)).max(),
            }
        }

        fn max_in_iter<'a>(types: impl IntoIterator<Item = &'a Term>) -> Option<usize> {
            types.into_iter().filter_map(max_in_type).max()
        }

        self.types.values().filter_map(max_in_type).max()
    }
}
