use neco_json::{JsonValue, parse_json};

#[test]
fn parse_scalars() {
    assert_eq!(parse_json("null").unwrap(), JsonValue::Null);
    assert_eq!(parse_json("true").unwrap(), JsonValue::Bool(true));
    assert_eq!(parse_json("false").unwrap(), JsonValue::Bool(false));
    assert_eq!(
        parse_json("-12.34e+2").unwrap(),
        JsonValue::Number("-12.34e+2".to_string())
    );
    assert_eq!(
        parse_json("\"a\\n\\u0041\"").unwrap(),
        JsonValue::String("a\nA".to_string())
    );
}

#[test]
fn parse_array_object() {
    let value = parse_json("{\"a\":[1,2],\"b\":{\"c\":null}}").unwrap();
    assert_eq!(
        value,
        JsonValue::Object(vec![
            (
                "a".to_string(),
                JsonValue::Array(vec![
                    JsonValue::Number("1".to_string()),
                    JsonValue::Number("2".to_string()),
                ]),
            ),
            (
                "b".to_string(),
                JsonValue::Object(vec![("c".to_string(), JsonValue::Null)]),
            ),
        ])
    );
}

#[test]
fn reject_trailing_comma() {
    assert!(parse_json("[1,]").is_err());
    assert!(parse_json("{\"a\":1,}").is_err());
}

#[test]
fn reject_leading_zero() {
    assert!(parse_json("01").is_err());
}
