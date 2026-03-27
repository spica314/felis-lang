use neco_rs_json::Error;

#[test]
fn formats_error_without_span() {
    let error = Error {
        span: None,
        message: "plain error".to_string(),
    };

    assert_eq!(error.to_string(), "plain error");
}
