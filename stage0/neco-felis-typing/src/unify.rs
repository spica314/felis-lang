use crate::{Type, TypeHole, UnificationError};
use std::collections::HashMap;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeSolutions {
    assignments: HashMap<TypeHole, Type>,
}

impl TypeSolutions {
    pub fn new() -> Self {
        Self {
            assignments: HashMap::new(),
        }
    }

    pub fn resolve(&self, ty: &Type) -> Type {
        match ty {
            Type::Hole(h) => {
                if let Some(replacement) = self.assignments.get(h) {
                    self.resolve(replacement)
                } else {
                    ty.clone()
                }
            }
            Type::Variable(_) => ty.clone(),
            Type::Arrow {
                param,
                param_name,
                result,
            } => Type::Arrow {
                param: Box::new(self.resolve(param)),
                param_name: param_name.clone(),
                result: Box::new(self.resolve(result)),
            },
            Type::Apply { function, argument } => Type::Apply {
                function: Box::new(self.resolve(function)),
                argument: Box::new(self.resolve(argument)),
            },
            Type::Struct(fields) => Type::Struct(
                fields
                    .iter()
                    .map(|field| crate::StructFieldType {
                        name: field.name.clone(),
                        ty: self.resolve(&field.ty),
                    })
                    .collect(),
            ),
            Type::Unit => Type::Unit,
            Type::Integer(int) => Type::Integer(*int),
            Type::F32 => Type::F32,
        }
    }

    pub fn apply_into(&self, ty: &mut Type) {
        *ty = self.resolve(ty);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&TypeHole, &Type)> {
        self.assignments.iter()
    }

    fn occurs(&self, hole: &TypeHole, ty: &Type) -> bool {
        match ty {
            Type::Hole(h) => {
                if h == hole {
                    true
                } else if let Some(replacement) = self.assignments.get(h) {
                    self.occurs(hole, replacement)
                } else {
                    false
                }
            }
            Type::Variable(_) | Type::Unit | Type::Integer(_) | Type::F32 => false,
            Type::Arrow { param, result, .. } => {
                self.occurs(hole, param) || self.occurs(hole, result)
            }
            Type::Apply { function, argument } => {
                self.occurs(hole, function) || self.occurs(hole, argument)
            }
            Type::Struct(fields) => fields.iter().any(|f| self.occurs(hole, &f.ty)),
        }
    }

    fn assign(&mut self, hole: TypeHole, ty: Type) -> Result<(), UnificationError> {
        if self.occurs(&hole, &ty) {
            return Err(UnificationError::Occurs { hole, ty });
        }
        self.assignments.insert(hole, ty);
        Ok(())
    }
}

pub struct UnificationCtx<'a> {
    solutions: &'a mut TypeSolutions,
}

impl<'a> UnificationCtx<'a> {
    pub fn new(solutions: &'a mut TypeSolutions) -> Self {
        Self { solutions }
    }

    pub fn unify(&mut self, lhs: &Type, rhs: &Type) -> Result<(), UnificationError> {
        let lhs = self.solutions.resolve(lhs);
        let rhs = self.solutions.resolve(rhs);

        match (lhs, rhs) {
            (Type::Hole(hole), ty) | (ty, Type::Hole(hole)) => self.solutions.assign(hole, ty),
            (Type::Variable(a), Type::Variable(b)) if a == b => Ok(()),
            (
                Type::Arrow {
                    param: param_lhs,
                    param_name: param_name_lhs,
                    result: result_lhs,
                },
                Type::Arrow {
                    param: param_rhs,
                    param_name: param_name_rhs,
                    result: result_rhs,
                },
            ) => {
                self.unify(&param_lhs, &param_rhs)?;
                if let (Some(l), Some(r)) = (param_name_lhs, param_name_rhs)
                    && l != r
                {
                    return Err(UnificationError::TypeMismatch {
                        expected: Type::Variable(l),
                        actual: Type::Variable(r),
                    });
                }
                self.unify(&result_lhs, &result_rhs)
            }
            (
                Type::Apply {
                    function: f_lhs,
                    argument: a_lhs,
                },
                Type::Apply {
                    function: f_rhs,
                    argument: a_rhs,
                },
            ) => {
                self.unify(&f_lhs, &f_rhs)?;
                self.unify(&a_lhs, &a_rhs)
            }
            (Type::Struct(fields_lhs), Type::Struct(fields_rhs)) => {
                if fields_lhs.len() != fields_rhs.len() {
                    return Err(UnificationError::TypeMismatch {
                        expected: Type::Struct(fields_lhs),
                        actual: Type::Struct(fields_rhs),
                    });
                }

                for (lhs_field, rhs_field) in fields_lhs.iter().zip(fields_rhs.iter()) {
                    if lhs_field.name != rhs_field.name {
                        return Err(UnificationError::TypeMismatch {
                            expected: Type::Struct(fields_lhs),
                            actual: Type::Struct(fields_rhs),
                        });
                    }
                    self.unify(&lhs_field.ty, &rhs_field.ty)?;
                }
                Ok(())
            }
            (Type::Unit, Type::Unit) | (Type::F32, Type::F32) => Ok(()),
            (Type::Integer(lhs), Type::Integer(rhs)) if lhs == rhs => Ok(()),
            (expected, actual) => Err(UnificationError::TypeMismatch { expected, actual }),
        }
    }
}
