use neco_rs_parser::{ArrowParameter, Term};

use crate::effect::Value;
use crate::ir::{ArrayElementType, ArrayKind, I32Expr, LoweredProgram, U8Expr};
use crate::{Error, Result};

pub(crate) fn validate_value_against_type(
    value: &Value,
    ty: &Term,
    program: &LoweredProgram,
) -> Result<()> {
    if let Some(element_type) = parse_raw_array_type_annotation(ty)? {
        return match value {
            Value::ByteString(_) if element_type == ArrayElementType::U8 => Ok(()),
            Value::Array {
                element_type: actual_element_type,
                kind: ArrayKind::Dynamic,
                ..
            } if *actual_element_type == element_type => Ok(()),
            Value::RuntimeArg(_) if element_type == ArrayElementType::U8 => Ok(()),
            _ => Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_term(ty)
            ))),
        };
    }

    if let Some((element_type, len)) = parse_array_type_annotation(ty)? {
        let Value::Array {
            slot,
            element_type: actual_element_type,
            kind: ArrayKind::Fixed,
        } = value
        else {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_term(ty)
            )));
        };
        if *actual_element_type != element_type {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_term(ty)
            )));
        }
        let actual_len = program
            .arrays
            .iter()
            .find(|array| array.slot == *slot)
            .map(|array| array.len)
            .ok_or_else(|| {
                Error::Unsupported(format!("unknown array slot `{slot}` while checking type"))
            })?;
        if actual_len != len {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_term(ty)
            )));
        }
        return Ok(());
    }

    if let Term::Reference {
        referent,
        exclusive: _,
    } = ty
    {
        return validate_reference_value_against_type(value, referent, program);
    }

    match ty {
        Term::Unit => {
            if matches!(value, Value::Unit) {
                Ok(())
            } else {
                Err(Error::Unsupported(format!(
                    "expected a value of type `()` but got {value:?}"
                )))
            }
        }
        Term::Path(path) if !path.starts_with_package && path.segments.len() == 1 => {
            let segment = &path.segments[0];
            if !segment.suffixes.is_empty() {
                return Ok(());
            }
            match segment.name.as_str() {
                "i32" if matches!(value, Value::I32(_)) => Ok(()),
                "u8" if matches!(value, Value::U8(_)) => Ok(()),
                "FileDescriptor" if matches!(value, Value::FileDescriptor(_)) => Ok(()),
                type_name => match value {
                    Value::Constructor(constructor) if constructor.type_name == type_name => Ok(()),
                    _ => Err(Error::Unsupported(format!(
                        "expected a value of type `{}` but got {value:?}",
                        render_term(ty)
                    ))),
                },
            }
        }
        _ => Ok(()),
    }
}

fn validate_reference_value_against_type(
    value: &Value,
    referent: &Term,
    program: &LoweredProgram,
) -> Result<()> {
    if let Some(element_type) = parse_raw_array_type_annotation(referent)? {
        return match value {
            Value::ByteString(_) if element_type == ArrayElementType::U8 => Ok(()),
            Value::Array {
                element_type: actual_element_type,
                kind: ArrayKind::Dynamic,
                ..
            } if *actual_element_type == element_type => Ok(()),
            Value::RuntimeArg(_) if element_type == ArrayElementType::U8 => Ok(()),
            _ => Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_reference_term(referent, false)
            ))),
        };
    }

    if let Some(element_type) = parse_unsized_array_type_annotation(referent)? {
        return match value {
            Value::ByteString(_) if element_type == ArrayElementType::U8 => Ok(()),
            Value::Array {
                element_type: actual_element_type,
                kind: ArrayKind::Dynamic,
                ..
            } if *actual_element_type == element_type => Ok(()),
            Value::RuntimeArg(_) if element_type == ArrayElementType::U8 => Ok(()),
            _ => Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_reference_term(referent, false)
            ))),
        };
    }

    if let Some((element_type, len)) = parse_array_type_annotation(referent)? {
        let Value::Array {
            slot,
            element_type: actual_element_type,
            kind: ArrayKind::Fixed,
        } = value
        else {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_reference_term(referent, false)
            )));
        };
        if *actual_element_type != element_type {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_reference_term(referent, false)
            )));
        }
        let actual_len = program
            .arrays
            .iter()
            .find(|array| array.slot == *slot)
            .map(|array| array.len)
            .ok_or_else(|| {
                Error::Unsupported(format!("unknown array slot `{slot}` while checking type"))
            })?;
        if actual_len != len {
            return Err(Error::Unsupported(format!(
                "expected a value of type `{}` but got {value:?}",
                render_reference_term(referent, false)
            )));
        }
        return Ok(());
    }

    match referent {
        Term::Path(path)
            if !path.starts_with_package
                && path.segments.len() == 1
                && path.segments[0].suffixes.is_empty() =>
        {
            match path.segments[0].name.as_str() {
                "i32" if matches!(value, Value::I32Reference(_)) => Ok(()),
                _ => Err(Error::Unsupported(format!(
                    "expected a value of type `{}` but got {value:?}",
                    render_reference_term(referent, false)
                ))),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "unsupported reference type `{}`",
            render_reference_term(referent, false)
        ))),
    }
}

fn parse_array_type_annotation(ty: &Term) -> Result<Option<(ArrayElementType, usize)>> {
    let Term::Application { callee, arguments } = ty else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.starts_with_package
        || path
            .segments
            .last()
            .is_none_or(|segment| segment.name != "Array")
        || path
            .segments
            .iter()
            .any(|segment| !segment.suffixes.is_empty())
    {
        return Ok(None);
    }

    let normalized = super::normalize_numeric_literal_arguments(arguments);
    let [element_type_term, len_term] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`Array` type annotation must receive an element type and a constant i32 length"
                .to_string(),
        ));
    };

    let element_type = parse_array_element_type(element_type_term, "Array")?;
    let len = parse_array_length_term(len_term)?;
    let len = usize::try_from(len)
        .map_err(|_| Error::Unsupported("`Array` length must be non-negative".to_string()))?;

    Ok(Some((element_type, len)))
}

fn parse_unsized_array_type_annotation(ty: &Term) -> Result<Option<ArrayElementType>> {
    let Term::Application { callee, arguments } = ty else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.starts_with_package
        || path
            .segments
            .last()
            .is_none_or(|segment| segment.name != "Array")
        || path
            .segments
            .iter()
            .any(|segment| !segment.suffixes.is_empty())
    {
        return Ok(None);
    }

    let [element_type_term] = arguments.as_slice() else {
        return Ok(None);
    };

    parse_array_element_type(element_type_term, "Array").map(Some)
}

fn parse_raw_array_type_annotation(ty: &Term) -> Result<Option<ArrayElementType>> {
    let Term::Application { callee, arguments } = ty else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    let Some(type_name) = path.segments.last().map(|segment| segment.name.as_str()) else {
        return Ok(None);
    };
    if type_name != "RawArray" {
        return Ok(None);
    }
    if path.starts_with_package
        || path
            .segments
            .iter()
            .any(|segment| !segment.suffixes.is_empty())
    {
        return Ok(None);
    }

    let [element_type_term] = arguments.as_slice() else {
        return Err(Error::Unsupported(format!(
            "`{type_name}` type annotation must receive exactly one element type"
        )));
    };

    parse_array_element_type(element_type_term, type_name).map(Some)
}

fn parse_array_element_type(term: &Term, type_name: &str) -> Result<ArrayElementType> {
    match term {
        Term::Path(path)
            if !path.starts_with_package
                && path.segments.len() == 1
                && path.segments[0].suffixes.is_empty() =>
        {
            match path.segments[0].name.as_str() {
                "i32" => Ok(ArrayElementType::I32),
                "u8" => Ok(ArrayElementType::U8),
                _ => Err(Error::Unsupported(format!(
                    "`{type_name}` element type must be `i32` or `u8`"
                ))),
            }
        }
        _ => Err(Error::Unsupported(format!(
            "`{type_name}` element type must be a simple path"
        ))),
    }
}

fn parse_array_length_term(term: &Term) -> Result<i32> {
    match term {
        Term::Application { callee, arguments } => {
            let [suffix] = arguments.as_slice() else {
                return Err(Error::Unsupported(
                    "`Array` length must be an `i32` literal".to_string(),
                ));
            };
            let Term::IntegerLiteral(literal) = callee.as_ref() else {
                return Err(Error::Unsupported(
                    "`Array` length must be an `i32` literal".to_string(),
                ));
            };
            if !is_i32_suffix_term(suffix) {
                return Err(Error::Unsupported(
                    "`Array` length must be an `i32` literal".to_string(),
                ));
            }
            parse_bare_i32_literal(literal).map(|expr| match expr {
                I32Expr::Literal(value) => value,
                _ => unreachable!(),
            })
        }
        _ => Err(Error::Unsupported(
            "`Array` length must be an `i32` literal".to_string(),
        )),
    }
}

pub(super) fn parse_suffixed_i32_literal(literal: &str) -> Result<I32Expr> {
    let digits = literal.strip_suffix("i32").ok_or_else(|| {
        Error::Unsupported("integer literal must use the `i32` suffix".to_string())
    })?;
    Ok(I32Expr::Literal(parse_i32_digits(digits, literal)?))
}

pub(super) fn parse_bare_i32_literal(literal: &str) -> Result<I32Expr> {
    Ok(I32Expr::Literal(parse_i32_digits(literal, literal)?))
}

pub(super) fn parse_suffixed_u8_literal(literal: &str) -> Result<U8Expr> {
    let digits = literal.strip_suffix("u8").ok_or_else(|| {
        Error::Unsupported("integer literal must use the `u8` suffix".to_string())
    })?;
    Ok(U8Expr::Literal(parse_u8_digits(digits, literal)?))
}

pub(super) fn parse_bare_u8_literal(literal: &str) -> Result<U8Expr> {
    Ok(U8Expr::Literal(parse_u8_digits(literal, literal)?))
}

fn parse_i32_digits(digits: &str, original: &str) -> Result<i32> {
    parse_prefixed_i32_digits(digits).map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as i32"
        ))
    })
}

fn parse_u8_digits(digits: &str, original: &str) -> Result<u8> {
    parse_prefixed_u8_digits(digits).map_err(|_| {
        Error::Unsupported(format!(
            "integer literal `{original}` could not be parsed as u8"
        ))
    })
}

fn parse_prefixed_i32_digits(digits: &str) -> std::result::Result<i32, std::num::ParseIntError> {
    if let Some(hex) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        i32::from_str_radix(hex, 16)
    } else {
        digits.parse::<i32>()
    }
}

fn parse_prefixed_u8_digits(digits: &str) -> std::result::Result<u8, std::num::ParseIntError> {
    if let Some(hex) = digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        u8::from_str_radix(hex, 16)
    } else {
        digits.parse::<u8>()
    }
}

fn render_term(term: &Term) -> String {
    match term {
        Term::Unit => "()".to_string(),
        Term::StringLiteral(value) => format!("{value:?}"),
        Term::CharLiteral(value) => format!("{value:?}"),
        Term::IntegerLiteral(value) => value.clone(),
        Term::Path(path) => {
            let mut rendered = String::new();
            if path.starts_with_package {
                rendered.push_str("#package");
                if !path.segments.is_empty() {
                    rendered.push_str("::");
                }
            }
            for (index, segment) in path.segments.iter().enumerate() {
                if index > 0 {
                    rendered.push_str("::");
                }
                rendered.push_str(&segment.name);
                for suffix in &segment.suffixes {
                    rendered.push('[');
                    rendered.push_str(&render_term(suffix));
                    rendered.push(']');
                }
            }
            rendered
        }
        Term::Group(inner) => format!("({})", render_term(inner)),
        Term::TypedBinder(binder) => format!("({} : {})", binder.name, render_term(&binder.ty)),
        Term::Block(_) => "{ ... }".to_string(),
        Term::Match(_) => "#match { ... }".to_string(),
        Term::Application { callee, arguments } => {
            let mut rendered = render_term(callee);
            for argument in arguments {
                rendered.push(' ');
                rendered.push_str(&render_term(argument));
            }
            rendered
        }
        Term::MethodCall { receiver, method } => format!("{} .> {}", render_term(receiver), method),
        Term::Reference {
            referent,
            exclusive,
        } => render_reference_term(referent, *exclusive),
        Term::Arrow(arrow) => {
            let lhs = match &arrow.parameter {
                ArrowParameter::Binder(binder) => {
                    format!("({} : {})", binder.name, render_term(&binder.ty))
                }
                ArrowParameter::Domain(domain) => render_term(domain),
            };
            format!("{lhs} -> {}", render_term(&arrow.result))
        }
        Term::Forall(forall) => {
            format!(
                "#forall {} : {}, {}",
                forall.binder.name,
                render_term(&forall.binder.ty),
                render_term(&forall.body)
            )
        }
    }
}

fn render_reference_term(referent: &Term, exclusive: bool) -> String {
    if exclusive {
        format!("&^ {}", render_term(referent))
    } else {
        format!("& {}", render_term(referent))
    }
}

pub(super) fn is_i32_suffix_term(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "i32",
        _ => false,
    }
}

pub(super) fn is_u8_suffix_term(term: &Term) -> bool {
    match term {
        Term::Path(path) => path.segments.len() == 1 && path.segments[0].name == "u8",
        _ => false,
    }
}

pub(super) fn nul_terminated_bytes(value: &str) -> Vec<u8> {
    let mut bytes = value.as_bytes().to_vec();
    bytes.push(0);
    bytes
}
