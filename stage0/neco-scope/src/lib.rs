pub struct Scope<K, V> {
    binds: Vec<(K, V)>,
}

impl<K, V> Scope<K, V> {
    pub fn set(&mut self, key: K, value: V) {
        self.binds.push((key, value));
    }

    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: PartialEq,
    {
        self.binds
            .iter()
            .rev()
            .find_map(|(k, v)| if k == key { Some(v) } else { None })
    }

    pub fn binds(&self) -> &[(K, V)] {
        &self.binds
    }
}

pub struct ScopeStack<K, V> {
    scopes: Vec<Scope<K, V>>,
}

impl<K, V> Default for ScopeStack<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> ScopeStack<K, V> {
    pub fn new() -> ScopeStack<K, V> {
        ScopeStack { scopes: Vec::new() }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope { binds: Vec::new() });
    }

    pub fn leave_scope(&mut self) -> Scope<K, V> {
        self.scopes
            .pop()
            .expect("Cannot leave scope: no scopes to leave")
    }

    pub fn set(&mut self, key: K, value: V) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.set(key, value);
        }
    }

    pub fn get(&self, key: &K) -> Option<&V>
    where
        K: PartialEq,
    {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(key) {
                return Some(value);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scope_set_adds_binding() {
        let mut scope = Scope { binds: Vec::new() };
        scope.set("key1", "value1");
        assert_eq!(scope.binds.len(), 1);
    }

    #[test]
    fn scope_get_finds_existing_key() {
        let mut scope = Scope { binds: Vec::new() };
        scope.set("key1", "value1");
        assert_eq!(scope.get(&"key1"), Some(&"value1"));
    }

    #[test]
    fn scope_get_returns_none_for_missing_key() {
        let scope = Scope::<&str, &str> { binds: Vec::new() };
        assert_eq!(scope.get(&"missing"), None);
    }

    #[test]
    fn scope_get_returns_most_recent_binding() {
        let mut scope = Scope { binds: Vec::new() };
        scope.set("key1", "value1");
        scope.set("key1", "value2");
        assert_eq!(scope.get(&"key1"), Some(&"value2"));
    }

    #[test]
    fn scope_binds_returns_all_bindings() {
        let mut scope = Scope { binds: Vec::new() };
        scope.set("key1", "value1");
        scope.set("key2", "value2");
        assert_eq!(scope.binds(), &[("key1", "value1"), ("key2", "value2")]);
    }

    #[test]
    fn scope_stack_new_creates_empty_stack() {
        let stack = ScopeStack::<&str, &str>::new();
        assert_eq!(stack.scopes.len(), 0);
    }

    #[test]
    fn scope_stack_enter_scope_adds_scope() {
        let mut stack = ScopeStack::<&str, &str>::new();
        stack.enter_scope();
        assert_eq!(stack.scopes.len(), 1);
    }

    #[test]
    fn scope_stack_leave_scope_removes_scope() {
        let mut stack = ScopeStack::<&str, &str>::new();
        stack.enter_scope();
        let _scope = stack.leave_scope();
        assert_eq!(stack.scopes.len(), 0);
    }

    #[test]
    fn scope_stack_leave_scope_returns_removed_scope() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("key1", "value1");
        let scope = stack.leave_scope();
        assert_eq!(scope.get(&"key1"), Some(&"value1"));
    }

    #[test]
    #[should_panic(expected = "Cannot leave scope: no scopes to leave")]
    fn scope_stack_leave_scope_panics_when_empty() {
        let mut stack = ScopeStack::<&str, &str>::new();
        stack.leave_scope();
    }

    #[test]
    fn scope_stack_set_adds_to_current_scope() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("key1", "value1");
        assert_eq!(stack.get(&"key1"), Some(&"value1"));
    }

    #[test]
    fn scope_stack_set_does_nothing_when_no_scopes() {
        let mut stack = ScopeStack::new();
        stack.set("key1", "value1");
        assert_eq!(stack.get(&"key1"), None);
    }

    #[test]
    fn scope_stack_get_searches_most_recent_scope_first() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("key1", "value1");
        stack.enter_scope();
        stack.set("key1", "value2");
        assert_eq!(stack.get(&"key1"), Some(&"value2"));
    }

    #[test]
    fn scope_stack_get_falls_back_to_outer_scopes() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("key1", "value1");
        stack.enter_scope();
        stack.set("key2", "value2");
        assert_eq!(stack.get(&"key1"), Some(&"value1"));
    }

    #[test]
    fn scope_stack_get_returns_none_when_key_not_found() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("key1", "value1");
        assert_eq!(stack.get(&"missing"), None);
    }

    #[test]
    fn scope_stack_shadowing_works_correctly() {
        let mut stack = ScopeStack::new();
        stack.enter_scope();
        stack.set("x", 1);
        stack.enter_scope();
        stack.set("x", 2);
        stack.enter_scope();
        stack.set("x", 3);

        assert_eq!(stack.get(&"x"), Some(&3));
        stack.leave_scope();
        assert_eq!(stack.get(&"x"), Some(&2));
        stack.leave_scope();
        assert_eq!(stack.get(&"x"), Some(&1));
    }
}
