use neco_rs_json::{JsonValue, TokenKind, parse};

#[test]
fn parses_nested_document() {
    let (tokens, value) = parse(
        r#"{
            "name": "hello\nworld",
            "version": 1.25e+2,
            "enabled": true,
            "items": [null, false, {"path": "a/b"}]
        }"#,
    )
    .expect("json parses");

    assert!(matches!(tokens.first().map(|token| &token.kind), Some(TokenKind::LeftBrace)));
    assert!(matches!(
        tokens.last().map(|token| &token.kind),
        Some(TokenKind::EndOfFile)
    ));

    let JsonValue::Object(entries) = value else {
        panic!("expected object");
    };

    assert_eq!(entries.len(), 4);
    assert_eq!(entries[0].key, "name");
    assert_eq!(entries[0].value, JsonValue::String("hello\nworld".to_string()));
    assert_eq!(entries[1].key, "version");
    assert_eq!(entries[1].value, JsonValue::Number("1.25e+2".to_string()));
    assert_eq!(entries[2].key, "enabled");
    assert_eq!(entries[2].value, JsonValue::Boolean(true));

    assert_eq!(entries[3].key, "items");
    let JsonValue::Array(items) = &entries[3].value else {
        panic!("expected array");
    };
    assert_eq!(items.len(), 3);
    assert_eq!(items[0], JsonValue::Null);
    assert_eq!(items[1], JsonValue::Boolean(false));

    let JsonValue::Object(nested_entries) = &items[2] else {
        panic!("expected nested object");
    };
    assert_eq!(nested_entries.len(), 1);
    assert_eq!(nested_entries[0].key, "path");
    assert_eq!(nested_entries[0].value, JsonValue::String("a/b".to_string()));
}

#[test]
fn parses_unicode_escape() {
    let (_, value) = parse(r#"{"text":"A\u3042\u2605"}"#).expect("json parses");

    let JsonValue::Object(entries) = value else {
        panic!("expected object");
    };

    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].key, "text");
    assert_eq!(entries[0].value, JsonValue::String("Aあ★".to_string()));
}

#[test]
fn parses_standard_string_escapes() {
    let (_, value) = parse(r#"{"text":"\"\\\/\b\f\n\r\t"}"#).expect("json parses");

    let JsonValue::Object(entries) = value else {
        panic!("expected object");
    };

    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].key, "text");
    assert_eq!(
        entries[0].value,
        JsonValue::String("\"\\/\u{0008}\u{000c}\n\r\t".to_string())
    );
}

#[test]
fn parses_negative_number() {
    let (_, value) = parse("-1").expect("json parses");

    assert_eq!(value, JsonValue::Number("-1".to_string()));
}
