#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JsonValue {
    Object(Vec<JsonEntry>),
    Array(Vec<JsonValue>),
    String(String),
    Number(String),
    Boolean(bool),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct JsonEntry {
    pub key: String,
    pub value: JsonValue,
}
