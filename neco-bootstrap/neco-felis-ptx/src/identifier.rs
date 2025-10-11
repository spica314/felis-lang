pub(crate) fn sanitize_ptx_identifier(raw: &str) -> String {
    let mut s = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            s.push(ch);
        } else {
            s.push('_');
        }
    }
    if s.is_empty()
        || !(s
            .chars()
            .next()
            .map(|c| c.is_ascii_alphabetic() || c == '_')
            .unwrap_or(false))
    {
        s.insert(0, '_');
    }
    s
}

pub(crate) fn compose_param_symbol(object: &str, field: Option<&str>) -> String {
    match field {
        Some(field_name) => format!(
            "{}_{}",
            sanitize_ptx_identifier(object),
            sanitize_ptx_identifier(field_name)
        ),
        None => sanitize_ptx_identifier(object),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanitize_ptx_identifier_rewrites_invalid_sequences() {
        assert_eq!(sanitize_ptx_identifier("thread.id"), "thread_id");
        assert_eq!(sanitize_ptx_identifier("1alpha"), "_1alpha");
        assert_eq!(sanitize_ptx_identifier("__ok__"), "__ok__");
        assert_eq!(sanitize_ptx_identifier(""), "_");
        assert_eq!(
            sanitize_ptx_identifier("pixel data/with-hyphen"),
            "pixel_data_with_hyphen"
        );
    }

    #[test]
    fn compose_param_symbol_sanitizes_components() {
        assert_eq!(
            compose_param_symbol("pixel data", Some("first-field!")),
            "pixel_data_first_field_"
        );
        assert_eq!(compose_param_symbol("pixel data", None), "pixel_data");
    }
}
