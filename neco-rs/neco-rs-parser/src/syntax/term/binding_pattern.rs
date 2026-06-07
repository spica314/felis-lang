#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingPattern {
    Name(String),
    Wildcard,
}
