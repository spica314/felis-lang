use crate::{Term, TypeHole, UnificationError};
use std::collections::HashMap;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TypeSolutions {
    assignments: HashMap<TypeHole, Term>,
}

impl TypeSolutions {
    pub fn new() -> Self {
        Self {
            assignments: HashMap::new(),
        }
    }

    pub fn resolve(&self, ty: &Term) -> Term {
        match ty {
            Term::Hole(h) => {
                if let Some(replacement) = self.assignments.get(h) {
                    self.resolve(replacement)
                } else {
                    ty.clone()
                }
            }
            Term::Variable(_) => ty.clone(),
            Term::Arrow {
                param,
                param_name,
                result,
            } => Term::Arrow {
                param: Box::new(self.resolve(param)),
                param_name: param_name.clone(),
                result: Box::new(self.resolve(result)),
            },
            Term::Apply { function, argument } => Term::Apply {
                function: Box::new(self.resolve(function)),
                argument: Box::new(self.resolve(argument)),
            },
            Term::Struct(fields) => Term::Struct(
                fields
                    .iter()
                    .map(|field| crate::StructFieldType {
                        name: field.name.clone(),
                        ty: self.resolve(&field.ty),
                    })
                    .collect(),
            ),
            Term::Unit => Term::Unit,
            Term::Integer(int) => Term::Integer(*int),
            Term::F32 => Term::F32,
        }
    }

    pub fn apply_into(&self, ty: &mut Term) {
        *ty = self.resolve(ty);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&TypeHole, &Term)> {
        self.assignments.iter()
    }

    fn occurs(&self, hole: &TypeHole, ty: &Term) -> bool {
        match ty {
            Term::Hole(h) => {
                if h == hole {
                    true
                } else if let Some(replacement) = self.assignments.get(h) {
                    self.occurs(hole, replacement)
                } else {
                    false
                }
            }
            Term::Variable(_) | Term::Unit | Term::Integer(_) | Term::F32 => false,
            Term::Arrow { param, result, .. } => {
                self.occurs(hole, param) || self.occurs(hole, result)
            }
            Term::Apply { function, argument } => {
                self.occurs(hole, function) || self.occurs(hole, argument)
            }
            Term::Struct(fields) => fields.iter().any(|f| self.occurs(hole, &f.ty)),
        }
    }

    fn assign(&mut self, hole: TypeHole, ty: Term) -> Result<(), UnificationError> {
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

    pub fn unify(&mut self, lhs: &Term, rhs: &Term) -> Result<(), UnificationError> {
        let lhs = self.solutions.resolve(lhs);
        let rhs = self.solutions.resolve(rhs);

        match (lhs, rhs) {
            (Term::Hole(hole), ty) | (ty, Term::Hole(hole)) => self.solutions.assign(hole, ty),
            (Term::Variable(a), Term::Variable(b)) if a == b => Ok(()),
            (
                Term::Arrow {
                    param: param_lhs,
                    param_name: param_name_lhs,
                    result: result_lhs,
                },
                Term::Arrow {
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
                        expected: Term::Variable(l),
                        actual: Term::Variable(r),
                    });
                }
                self.unify(&result_lhs, &result_rhs)
            }
            (
                Term::Apply {
                    function: f_lhs,
                    argument: a_lhs,
                },
                Term::Apply {
                    function: f_rhs,
                    argument: a_rhs,
                },
            ) => {
                self.unify(&f_lhs, &f_rhs)?;
                self.unify(&a_lhs, &a_rhs)
            }
            (Term::Struct(fields_lhs), Term::Struct(fields_rhs)) => {
                if fields_lhs.len() != fields_rhs.len() {
                    return Err(UnificationError::TypeMismatch {
                        expected: Term::Struct(fields_lhs),
                        actual: Term::Struct(fields_rhs),
                    });
                }

                for (lhs_field, rhs_field) in fields_lhs.iter().zip(fields_rhs.iter()) {
                    if lhs_field.name != rhs_field.name {
                        return Err(UnificationError::TypeMismatch {
                            expected: Term::Struct(fields_lhs),
                            actual: Term::Struct(fields_rhs),
                        });
                    }
                    self.unify(&lhs_field.ty, &rhs_field.ty)?;
                }
                Ok(())
            }
            (Term::Unit, Term::Unit) | (Term::F32, Term::F32) => Ok(()),
            (Term::Integer(lhs), Term::Integer(rhs)) if lhs == rhs => Ok(()),
            (expected, actual) => Err(UnificationError::TypeMismatch { expected, actual }),
        }
    }
}
