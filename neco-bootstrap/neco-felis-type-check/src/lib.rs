use std::collections::HashMap;
use std::rc::Rc;

use neco_cic::{
    global_environment::GlobalEnvironment,
    id::{Id, IdGenerator},
    inductive::{ConstructorDefinition, InductiveDefinition},
    local_context::LocalContext,
    reduction,
    term::{
        Sort, Term, TermApplication, TermConstant, TermMatch, TermMatchBranch, TermProduct,
        TermSort, TermVariable,
    },
    typechecker,
};

use neco_felis_syn::{
    File, FileIdGenerator, Item, ItemDefinition, ItemInductive, ItemTheorem, Parse, Pattern,
    PhaseParse, Term as FTerm, TermMatch as FTermMatch, TermMatchBranch as FTermMatchBranch,
    token::Token,
};

pub struct TypeChecker {
    id_gen: IdGenerator,
    global_env: GlobalEnvironment,
    name_to_id: HashMap<String, Id>,
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            id_gen: IdGenerator::new(),
            global_env: GlobalEnvironment::new(),
            name_to_id: HashMap::new(),
        }
    }

    pub fn check_file(&mut self, file: &File<PhaseParse>) -> Result<(), String> {
        for item in file.items() {
            self.process_item(item)?;
        }
        Ok(())
    }

    fn process_item(&mut self, item: &Item<PhaseParse>) -> Result<(), String> {
        match item {
            Item::Inductive(inductive) => self.process_inductive(inductive),
            Item::Definition(definition) => self.process_definition(definition),
            Item::Theorem(theorem) => self.process_theorem(theorem),
            Item::Entrypoint(_entrypoint) => {
                // Entrypoint items are handled separately and don't need type checking
                Ok(())
            }
            Item::UseBuiltin(_use_builtin) => {
                // Builtin items are handled separately and don't need type checking
                Ok(())
            }
            Item::Proc(_item_proc) => Ok(()),
            Item::Struct(_item_struct) => {
                // TODO: Implement struct type checking
                Ok(())
            }
        }
    }

    fn process_inductive(&mut self, inductive: &ItemInductive<PhaseParse>) -> Result<(), String> {
        let name = inductive.name().s();
        let id = self.id_gen.generate_id();
        self.name_to_id.insert(name.to_string(), id);

        // Convert type
        let type_term = self.convert_term(inductive.ty())?;

        // Convert constructors
        let mut constructors = Vec::new();
        for branch in inductive.branches() {
            let constructor_id = self.id_gen.generate_id();
            let constructor_name = branch.name().s();
            self.name_to_id
                .insert(constructor_name.to_string(), constructor_id);

            let constructor_type = self.convert_term(branch.ty())?;
            constructors.push(ConstructorDefinition {
                name: constructor_id,
                ty: Rc::new(constructor_type),
                arity: 0, // TODO: compute actual arity
            });
        }

        let inductive_def = InductiveDefinition::new(
            id,
            Vec::new(), // No parameters for now
            Rc::new(type_term),
            constructors,
        );

        self.global_env
            .inductives
            .add_inductive(inductive_def)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn process_definition(
        &mut self,
        definition: &ItemDefinition<PhaseParse>,
    ) -> Result<(), String> {
        let name = definition.name().s();
        let id = self.id_gen.generate_id();
        self.name_to_id.insert(name.to_string(), id);

        let type_term = self.convert_term(definition.type_())?;
        let body_term = self.convert_term(definition.body())?;

        // Convert type into lambda abstraction
        let lambda_body = Self::create_lambda_from_product(&type_term, body_term)?;

        println!("Processing definition: {name}");

        let const_def = neco_cic::global_environment::ConstantDefinition {
            name: id,
            body: Some(Rc::new(lambda_body)),
            ty: Rc::new(type_term),
        };
        self.global_env
            .add_constant(const_def)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn process_theorem(&mut self, theorem: &ItemTheorem<PhaseParse>) -> Result<(), String> {
        let name = theorem.name().s();
        let id = self.id_gen.generate_id();
        self.name_to_id.insert(name.to_string(), id);

        println!("Processing theorem: {name}");

        // Convert the theorem type and proof
        let theorem_type = self.convert_term(theorem.type_())?;
        let proof_term = self.convert_term(theorem.body())?;

        // Type check the proof against the theorem type using CIC type checker
        let local_ctx = LocalContext::new();

        // Get the type of the proof term
        let proof_type = typechecker::infer_type(&local_ctx, &self.global_env, &proof_term)
            .map_err(|e| format!("Failed to infer proof type: {e}"))?;

        // Check if the reduced theorem type equals the reduced proof type
        let theorem_type_reduced = reduction::normalize_with_env(&theorem_type, &self.global_env);
        let proof_type_reduced = reduction::normalize_with_env(&proof_type, &self.global_env);

        if !typechecker::is_convertible(
            &local_ctx,
            &self.global_env,
            &theorem_type_reduced,
            &proof_type_reduced,
        ) {
            return Err(format!(
                "Theorem type does not match proof type for: {name}\nTheorem type (reduced): {theorem_type_reduced:?}\nProof type (reduced): {proof_type_reduced:?}"
            ));
        }

        let const_def = neco_cic::global_environment::ConstantDefinition {
            name: id,
            body: Some(Rc::new(proof_term)),
            ty: Rc::new(theorem_type),
        };
        self.global_env
            .add_constant(const_def)
            .map_err(|e| e.to_string())?;
        Ok(())
    }

    fn convert_term(&mut self, term: &FTerm<PhaseParse>) -> Result<Term, String> {
        match term {
            FTerm::Variable(var) => {
                let name = var.variable().s();

                // Handle built-in types
                match name {
                    "Type" => Ok(Term::Sort(TermSort {
                        sort: Sort::Type(0),
                    })),
                    "Set" => Ok(Term::Sort(TermSort { sort: Sort::Set })),
                    "Prop" => Ok(Term::Sort(TermSort { sort: Sort::Prop })),
                    _ => {
                        if let Some(&id) = self.name_to_id.get(name) {
                            Ok(Term::Constant(TermConstant { id }))
                        } else {
                            Err(format!("Unknown variable: {name}"))
                        }
                    }
                }
            }
            FTerm::Apply(apply) => {
                let f = self.convert_term(apply.f())?;
                let mut args = Vec::new();
                for arg in apply.args() {
                    args.push(self.convert_term(arg)?);
                }
                Ok(Term::Application(TermApplication {
                    f: Rc::new(f),
                    args,
                }))
            }
            FTerm::ArrowDep(arrow) => {
                let var_name = arrow.from().variable().s();
                let var_id = self.id_gen.generate_id();
                self.name_to_id.insert(var_name.to_string(), var_id);

                let source = self.convert_term(arrow.from_ty())?;
                let target = self.convert_term(arrow.to())?;

                Ok(Term::Product(TermProduct {
                    var: var_id,
                    source: Rc::new(source),
                    target: Rc::new(target),
                }))
            }
            FTerm::ArrowNodep(arrow) => {
                let dummy_var = self.id_gen.generate_id();
                let source = self.convert_term(arrow.from())?;
                let target = self.convert_term(arrow.to())?;

                Ok(Term::Product(TermProduct {
                    var: dummy_var,
                    source: Rc::new(source),
                    target: Rc::new(target),
                }))
            }
            FTerm::Paren(paren) => self.convert_term(paren.term()),
            FTerm::Match(match_expr) => self.convert_match(match_expr),
            FTerm::Unit(_term_unit) => todo!(),
            FTerm::Number(_term_number) => todo!(),
            FTerm::Struct(_item_struct) => {
                // For now, struct expressions are not supported in the type checker
                // They should be handled at the compilation level
                Err("Struct expressions are not supported in type checker yet".to_string())
            }
        }
    }

    fn convert_match(&mut self, match_expr: &FTermMatch<PhaseParse>) -> Result<Term, String> {
        let scrutinee_name = match_expr.scrutinee().s();
        let scrutinee_id = self
            .name_to_id
            .get(scrutinee_name)
            .ok_or_else(|| format!("Unknown variable in match: {scrutinee_name}"))?;

        let scrutinee = Term::Variable(TermVariable { id: *scrutinee_id });

        // For now, use a simple return type (we'd need more sophisticated inference)
        let return_type = Term::Sort(TermSort { sort: Sort::Set });

        let mut branches = Vec::new();
        for branch in match_expr.branches() {
            let cic_branch = self.convert_match_branch(branch)?;
            branches.push(cic_branch);
        }

        Ok(Term::Match(TermMatch {
            scrutinee: Rc::new(scrutinee),
            return_type: Rc::new(return_type),
            branches,
        }))
    }

    fn convert_match_branch(
        &mut self,
        branch: &FTermMatchBranch<PhaseParse>,
    ) -> Result<TermMatchBranch, String> {
        match branch.pattern() {
            Pattern::Variable(var) => {
                let constructor_name = var.s();
                let constructor_id = *self
                    .name_to_id
                    .get(constructor_name)
                    .ok_or_else(|| format!("Unknown constructor: {constructor_name}"))?;

                let body = self.convert_term(branch.body())?;

                Ok(TermMatchBranch {
                    constructor_id,
                    bound_vars: Vec::new(),
                    body: Rc::new(body),
                })
            }
            Pattern::Constructor(constructor, args) => {
                let constructor_name = constructor.s();
                let constructor_id = *self
                    .name_to_id
                    .get(constructor_name)
                    .ok_or_else(|| format!("Unknown constructor: {constructor_name}"))?;

                let mut bound_vars = Vec::new();
                for arg in args {
                    let arg_id = self.id_gen.generate_id();
                    self.name_to_id.insert(arg.s().to_string(), arg_id);
                    bound_vars.push(arg_id);
                }

                let body = self.convert_term(branch.body())?;

                Ok(TermMatchBranch {
                    constructor_id,
                    bound_vars,
                    body: Rc::new(body),
                })
            }
        }
    }

    /// Creates lambda abstractions from a product type
    /// For example: (n : nat) -> (m : nat) -> nat with body becomes
    /// λn:nat. λm:nat. body
    fn create_lambda_from_product(product_type: &Term, body: Term) -> Result<Term, String> {
        use neco_cic::term::TermLambda;

        match product_type {
            Term::Product(product) => {
                // Create lambda for this parameter
                let inner_lambda = Self::create_lambda_from_product(&product.target, body)?;
                Ok(Term::Lambda(TermLambda {
                    var: product.var,
                    source_ty: product.source.clone(),
                    target: Rc::new(inner_lambda),
                }))
            }
            _ => {
                // Base case: no more products, return the body
                Ok(body)
            }
        }
    }
}

pub fn type_check_file(file_contents: &str) -> Result<(), String> {
    let mut file_id_generator = FileIdGenerator::new();
    let file_id = file_id_generator.generate_file_id();
    let tokens = Token::lex(file_contents, file_id);

    let mut i = 0;
    let file = File::parse(&tokens, &mut i)
        .map_err(|e| format!("Parse error: {e:?}"))?
        .ok_or("Failed to parse file")?;

    let mut type_checker = TypeChecker::new();
    type_checker.check_file(&file)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_check_eq_and_nat() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat.fe").unwrap();
        let result = type_check_file(&file_contents);
        assert!(result.is_ok(), "Type checking failed: {result:?}");
    }

    #[test]
    fn test_type_check_eq_and_nat_fail_1() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_fail_1.fe").unwrap();
        let result = type_check_file(&file_contents);
        eprintln!("result = {result:?}");
        assert!(
            result.is_err(),
            "Type checking should have failed but succeeded: {result:?}"
        );
    }

    #[test]
    fn test_type_check_eq_and_nat_fail_2() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_fail_2.fe").unwrap();
        let result = type_check_file(&file_contents);
        assert!(
            result.is_err(),
            "Type checking should have failed but succeeded: {result:?}"
        );
    }

    #[test]
    fn test_type_check_eq_and_nat_fail_3() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_fail_3.fe").unwrap();
        let result = type_check_file(&file_contents);
        assert!(
            result.is_err(),
            "Type checking should have failed but succeeded: {result:?}"
        );
    }

    #[test]
    fn test_type_check_eq_and_nat_fail_4() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_fail_4.fe").unwrap();
        let result = type_check_file(&file_contents);
        assert!(
            result.is_err(),
            "Type checking should have failed but succeeded: {result:?}"
        );
    }

    #[test]
    fn test_type_check_eq_and_nat_correct_2() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_correct_2.fe")
                .unwrap();
        let result = type_check_file(&file_contents);
        assert!(result.is_ok(), "Type checking failed: {result:?}");
    }

    #[test]
    fn test_type_check_eq_and_nat_correct_3() {
        let file_contents =
            std::fs::read_to_string("../../testcases/felis/single/eq_and_nat_correct_3.fe")
                .unwrap();
        let result = type_check_file(&file_contents);
        assert!(result.is_ok(), "Type checking failed: {result:?}");
    }

    #[test]
    fn test_theorem_type_matches_proof_type() {
        let mut type_checker = TypeChecker::new();
        let x = type_checker.id_gen.generate_id();
        let proof_term = Term::Constant(TermConstant { id: x });
        let theorem_type = Term::Sort(TermSort { sort: Sort::Set });
        let local_ctx = LocalContext::new();

        // Add x : Set to the context by creating a constant
        let const_def = neco_cic::global_environment::ConstantDefinition {
            name: x,
            body: None,
            ty: Rc::new(theorem_type.clone()),
        };
        type_checker.global_env.add_constant(const_def).unwrap();

        // Infer the type of the proof term
        let proof_type =
            typechecker::infer_type(&local_ctx, &type_checker.global_env, &proof_term).unwrap();

        // Check that the types are convertible after reduction
        let theorem_reduced = reduction::normalize(&theorem_type);
        let proof_reduced = reduction::normalize(&proof_type);

        assert!(typechecker::is_convertible(
            &local_ctx,
            &type_checker.global_env,
            &theorem_reduced,
            &proof_reduced
        ));
    }

    #[test]
    fn test_theorem_type_mismatch_proof_type() {
        let mut type_checker = TypeChecker::new();
        let x = type_checker.id_gen.generate_id();
        let proof_term = Term::Constant(TermConstant { id: x });
        let theorem_type = Term::Sort(TermSort { sort: Sort::Prop });
        let proof_type_decl = Term::Sort(TermSort { sort: Sort::Set });
        let local_ctx = LocalContext::new();

        // Add x : Set to the context (different from what theorem expects)
        let const_def = neco_cic::global_environment::ConstantDefinition {
            name: x,
            body: None,
            ty: Rc::new(proof_type_decl),
        };
        type_checker.global_env.add_constant(const_def).unwrap();

        // Infer the type of the proof term
        let proof_type =
            typechecker::infer_type(&local_ctx, &type_checker.global_env, &proof_term).unwrap();

        // Check that the types are NOT convertible after reduction
        let theorem_reduced = reduction::normalize(&theorem_type);
        let proof_reduced = reduction::normalize(&proof_type);

        assert!(!typechecker::is_convertible(
            &local_ctx,
            &type_checker.global_env,
            &theorem_reduced,
            &proof_reduced
        ));
    }
}
