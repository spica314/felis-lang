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
}
