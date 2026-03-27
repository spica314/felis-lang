use neco_rs_json::{Span, parse};

fn assert_parse_error(source: &str, message: &str, span: Span) {
    let error = parse(source).expect_err("parse should fail");
    assert_eq!(error.message, message);
    assert_eq!(error.span, Some(span));
}

#[test]
fn reports_trailing_tokens() {
    let error = parse(r#"{"name":"neco"} []"#).expect_err("parse should fail");

    assert_eq!(error.message, "unexpected trailing tokens");
    assert_eq!(error.span, Some(Span { start: 16, end: 17 }));
    assert_eq!(error.to_string(), "16..17: unexpected trailing tokens");
}

#[test]
fn reports_missing_object_value() {
    let error = parse(r#"{"name":}"#).expect_err("parse should fail");

    assert_eq!(error.message, "expected JSON value");
    assert_eq!(error.span, Some(Span { start: 8, end: 9 }));
}

#[test]
fn reports_unsupported_escape_sequence() {
    let error = parse(r#"{"text":"\q"}"#).expect_err("parse should fail");

    assert_eq!(error.message, r#"unsupported escape sequence `\q`"#);
    assert_eq!(error.span, Some(Span { start: 8, end: 11 }));
}

#[test]
fn reports_unterminated_escape_sequence() {
    let error = parse("{\"text\":\"\\").expect_err("parse should fail");

    assert_eq!(error.message, "unterminated string literal");
    assert_eq!(error.span, Some(Span { start: 8, end: 10 }));
}

#[test]
fn reports_non_digit_after_minus() {
    let error = parse("-x").expect_err("parse should fail");

    assert_eq!(error.message, "expected digit after `-`");
    assert_eq!(error.span, Some(Span { start: 0, end: 1 }));
}

#[test]
fn reports_parser_error_patterns() {
    assert_parse_error(
        r#"{true:1}"#,
        "expected object key string",
        Span { start: 1, end: 5 },
    );
    assert_parse_error(r#"{"a" 1}"#, "expected `:`", Span { start: 5, end: 6 });
    assert_parse_error(r#"{"a":1"#, "expected `}`", Span { start: 6, end: 6 });
    assert_parse_error(r#"[1"#, "expected `]`", Span { start: 2, end: 2 });
}

#[test]
fn reports_lexer_error_patterns() {
    assert_parse_error("@", "unexpected character `@`", Span { start: 0, end: 1 });
    assert_parse_error(
        "\"",
        "unterminated string literal",
        Span { start: 0, end: 1 },
    );
    assert_parse_error(
        "\"\\u12",
        "unterminated unicode escape",
        Span { start: 0, end: 5 },
    );
    assert_parse_error(
        "\"\\u12xz\"",
        "unicode escape must contain four hex digits",
        Span { start: 0, end: 6 },
    );
    assert_parse_error(
        "t",
        "unexpected end of input after `t`",
        Span { start: 0, end: 1 },
    );
    assert_parse_error("tre", "unexpected token `tre`", Span { start: 0, end: 3 });
    assert_parse_error("-", "expected digit after `-`", Span { start: 0, end: 1 });
    assert_parse_error(
        "1.",
        "expected digit after decimal point",
        Span { start: 0, end: 2 },
    );
    assert_parse_error(
        "1e+",
        "expected digit in exponent",
        Span { start: 0, end: 3 },
    );
    assert_parse_error(
        r#""\uD800""#,
        "unicode escape is not a valid scalar value",
        Span { start: 0, end: 7 },
    );
}
