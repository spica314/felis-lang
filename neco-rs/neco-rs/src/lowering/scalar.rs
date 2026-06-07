use crate::ir::ComparisonKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ScalarType {
    I32,
    I64,
    F32,
    U8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ScalarBinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Xor,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ScalarUnaryOp {
    Sqrt,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum ScalarPrimitive {
    Binary {
        ty: ScalarType,
        op: ScalarBinaryOp,
    },
    Unary {
        ty: ScalarType,
        op: ScalarUnaryOp,
    },
    Conversion {
        source: ScalarType,
        dest: ScalarType,
    },
    Comparison {
        ty: ScalarType,
        kind: ComparisonKind,
    },
}

impl ScalarPrimitive {
    pub(super) fn arity(self) -> usize {
        match self {
            Self::Binary { .. } | Self::Comparison { .. } => 2,
            Self::Unary { .. } | Self::Conversion { .. } => 1,
        }
    }
}

pub(super) fn scalar_primitive(name: &str) -> Option<ScalarPrimitive> {
    let (ty, suffix) = scalar_prefix(name)?;
    match (ty, suffix) {
        (_, "add") => binary(ty, ScalarBinaryOp::Add),
        (_, "sub") => binary(ty, ScalarBinaryOp::Sub),
        (_, "mul") => binary(ty, ScalarBinaryOp::Mul),
        (ScalarType::I32 | ScalarType::I64 | ScalarType::U8, "div") => {
            binary(ty, ScalarBinaryOp::Div)
        }
        (ScalarType::F32, "div") => binary(ty, ScalarBinaryOp::Div),
        (ScalarType::I32 | ScalarType::I64 | ScalarType::U8, "mod") => {
            binary(ty, ScalarBinaryOp::Mod)
        }
        (ScalarType::I32, "xor") => binary(ty, ScalarBinaryOp::Xor),
        (ScalarType::I32, "shl") => binary(ty, ScalarBinaryOp::Shl),
        (ScalarType::I32, "shr") => binary(ty, ScalarBinaryOp::Shr),
        (ScalarType::F32, "sqrt") => Some(ScalarPrimitive::Unary {
            ty,
            op: ScalarUnaryOp::Sqrt,
        }),
        _ if suffix.starts_with("from_") => {
            let source = scalar_type(&suffix["from_".len()..])?;
            if source == ty {
                return None;
            }
            Some(ScalarPrimitive::Conversion { source, dest: ty })
        }
        _ => comparison_kind(suffix).map(|kind| ScalarPrimitive::Comparison { ty, kind }),
    }
}

fn binary(ty: ScalarType, op: ScalarBinaryOp) -> Option<ScalarPrimitive> {
    Some(ScalarPrimitive::Binary { ty, op })
}

fn scalar_prefix(name: &str) -> Option<(ScalarType, &str)> {
    for (prefix, ty) in [
        ("i32_", ScalarType::I32),
        ("i64_", ScalarType::I64),
        ("f32_", ScalarType::F32),
        ("u8_", ScalarType::U8),
    ] {
        if let Some(suffix) = name.strip_prefix(prefix) {
            return Some((ty, suffix));
        }
    }
    None
}

fn scalar_type(name: &str) -> Option<ScalarType> {
    match name {
        "i32" => Some(ScalarType::I32),
        "i64" => Some(ScalarType::I64),
        "f32" => Some(ScalarType::F32),
        "u8" => Some(ScalarType::U8),
        _ => None,
    }
}

fn comparison_kind(suffix: &str) -> Option<ComparisonKind> {
    match suffix {
        "eq" => Some(ComparisonKind::Eq),
        "lte" => Some(ComparisonKind::Lte),
        "lt" => Some(ComparisonKind::Lt),
        "gte" => Some(ComparisonKind::Gte),
        "gt" => Some(ComparisonKind::Gt),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::{ScalarBinaryOp, ScalarPrimitive, ScalarType, ScalarUnaryOp, scalar_primitive};
    use crate::ir::ComparisonKind;

    #[test]
    fn scalar_primitive_describes_comparisons_by_suffix() {
        assert_eq!(
            scalar_primitive("i32_lte"),
            Some(ScalarPrimitive::Comparison {
                ty: ScalarType::I32,
                kind: ComparisonKind::Lte
            })
        );
        assert_eq!(
            scalar_primitive("u8_gt"),
            Some(ScalarPrimitive::Comparison {
                ty: ScalarType::U8,
                kind: ComparisonKind::Gt
            })
        );
    }

    #[test]
    fn scalar_primitive_describes_arithmetic_and_special_ops() {
        assert_eq!(
            scalar_primitive("i64_mod"),
            Some(ScalarPrimitive::Binary {
                ty: ScalarType::I64,
                op: ScalarBinaryOp::Mod
            })
        );
        assert_eq!(
            scalar_primitive("f32_sqrt"),
            Some(ScalarPrimitive::Unary {
                ty: ScalarType::F32,
                op: ScalarUnaryOp::Sqrt
            })
        );
    }

    #[test]
    fn scalar_primitive_describes_conversions() {
        assert_eq!(
            scalar_primitive("f32_from_u8"),
            Some(ScalarPrimitive::Conversion {
                source: ScalarType::U8,
                dest: ScalarType::F32
            })
        );
    }

    #[test]
    fn scalar_primitive_rejects_unsupported_type_operation_pairs() {
        assert_eq!(scalar_primitive("f32_mod"), None);
        assert_eq!(scalar_primitive("u8_shl"), None);
        assert_eq!(scalar_primitive("i32_sqrt"), None);
        assert_eq!(scalar_primitive("i64_from_i64"), None);
    }
}
