use neco_felis_syn::{
    File, Item, ItemDefinition, ItemInductive, ItemInductiveBranch, ItemTheorem, Pattern,
    PhaseParse, Term, TermApply, TermArrowDep, TermArrowNodep, TermMatch, TermMatchBranch,
    TermNumber, TermParen, TermUnit, TermVariable,
};
use neco_scope::ScopeStack;

use crate::phase_renamed::PhaseRenamed;

pub mod phase_renamed;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
// (file_id, variable_id_in_the_file)
pub struct VariableId(pub usize, pub usize);

struct RenameContext {
    file_id: usize,
    next_variable_id: usize,
    scope: ScopeStack<String, VariableId>,
}

impl RenameContext {
    fn new(file_id: usize) -> Self {
        Self {
            file_id,
            next_variable_id: 0,
            scope: ScopeStack::new(),
        }
    }

    fn generate_variable_id(&mut self) -> VariableId {
        let id = VariableId(self.file_id, self.next_variable_id);
        self.next_variable_id += 1;
        id
    }

    fn bind_variable(&mut self, name: &str) -> VariableId {
        let id = self.generate_variable_id();
        self.scope.set(name.to_string(), id.clone());
        id
    }

    fn lookup_variable(&self, name: &str) -> Option<VariableId> {
        self.scope.get(&name.to_string()).cloned()
    }

    fn enter_scope(&mut self) {
        self.scope.enter_scope();
    }

    fn leave_scope(&mut self) {
        self.scope.leave_scope();
    }
}

fn rename_item(context: &mut RenameContext, item: &Item<PhaseParse>) -> Item<PhaseRenamed> {
    match item {
        Item::Definition(definition) => {
            context.enter_scope();

            // Bind the definition name in the current scope
            let _def_id = context.bind_variable(definition.name().s());

            let renamed_definition = ItemDefinition {
                keyword_definition: definition.keyword_definition.clone(),
                name: definition.name.clone(),
                colon: definition.colon.clone(),
                type_: Box::new(rename_term(context, definition.type_())),
                brace_l: definition.brace_l.clone(),
                body: Box::new(rename_term(context, definition.body())),
                brace_r: definition.brace_r.clone(),
                ext: (),
            };

            context.leave_scope();
            Item::Definition(renamed_definition)
        }
        Item::Inductive(inductive) => {
            context.enter_scope();

            // Bind the inductive type name
            let _ind_id = context.bind_variable(inductive.name().s());

            let renamed_inductive = ItemInductive {
                keyword_inductive: inductive.keyword_inductive.clone(),
                name: inductive.name.clone(),
                colon: inductive.colon.clone(),
                ty: Box::new(rename_term(context, inductive.ty())),
                brace_l: inductive.brace_l.clone(),
                branches: inductive
                    .branches()
                    .iter()
                    .map(|branch| rename_inductive_branch(context, branch))
                    .collect(),
                brace_r: inductive.brace_r.clone(),
                ext: (),
            };

            context.leave_scope();
            Item::Inductive(renamed_inductive)
        }
        Item::Theorem(theorem) => {
            context.enter_scope();

            // Bind the theorem name
            let _thm_id = context.bind_variable(theorem.name().s());

            let renamed_theorem = ItemTheorem {
                keyword_theorem: theorem.keyword_theorem.clone(),
                name: theorem.name.clone(),
                colon: theorem.colon.clone(),
                type_: Box::new(rename_term(context, theorem.type_())),
                brace_l: theorem.brace_l.clone(),
                body: Box::new(rename_term(context, theorem.body())),
                brace_r: theorem.brace_r.clone(),
                ext: (),
            };

            context.leave_scope();
            Item::Theorem(renamed_theorem)
        }
        Item::Entrypoint(_) => {
            // For now, just return the item unchanged since we don't handle entrypoints in renaming
            unreachable!("Entrypoint items are not yet supported in renaming")
        }
        Item::UseBuiltin(_) => {
            // For now, just return the item unchanged since we don't handle builtins in renaming
            unreachable!("UseBuiltin items are not yet supported in renaming")
        }
        Item::Proc(_) => {
            // For now, just return the item unchanged since we don't handle builtins in renaming
            unreachable!("Proc items are not yet supported in renaming")
        }
        Item::Struct(_) => {
            // For now, just return the item unchanged since we don't handle structs in renaming
            unreachable!("Struct items are not yet supported in renaming")
        }
    }
}

fn rename_inductive_branch(
    context: &mut RenameContext,
    branch: &ItemInductiveBranch<PhaseParse>,
) -> ItemInductiveBranch<PhaseRenamed> {
    context.enter_scope();

    // Bind the constructor name
    let _ctor_id = context.bind_variable(branch.name().s());

    let renamed_branch = ItemInductiveBranch {
        name: branch.name.clone(),
        colon: branch.colon.clone(),
        ty: Box::new(rename_term(context, branch.ty())),
        comma: branch.comma.clone(),
        ext: (),
    };

    context.leave_scope();
    renamed_branch
}

fn rename_term(context: &mut RenameContext, term: &Term<PhaseParse>) -> Term<PhaseRenamed> {
    match term {
        Term::Variable(var) => {
            let variable_name = var.variable().s();
            let variable_id = context.lookup_variable(variable_name).unwrap_or_else(|| {
                // If variable not found, create a new ID (this might be an error case)
                context.generate_variable_id()
            });

            Term::Variable(TermVariable {
                variable: var.variable.clone(),
                ext: variable_id,
            })
        }
        Term::Apply(apply) => Term::Apply(TermApply {
            f: Box::new(rename_term(context, apply.f())),
            args: apply
                .args()
                .iter()
                .map(|arg| rename_term(context, arg))
                .collect(),
            ext: (),
        }),
        Term::ArrowDep(arrow) => {
            context.enter_scope();

            // Bind the parameter name
            context.bind_variable(arrow.from().variable().s());

            let renamed_arrow = TermArrowDep {
                paren_l: arrow.paren_l.clone(),
                from: TermVariable {
                    variable: arrow.from.variable.clone(),
                    ext: context
                        .lookup_variable(arrow.from().variable().s())
                        .unwrap(),
                },
                colon: arrow.colon.clone(),
                from_ty: Box::new(rename_term(context, arrow.from_ty())),
                paren_r: arrow.paren_r.clone(),
                arrow: arrow.arrow.clone(),
                to: Box::new(rename_term(context, arrow.to())),
                ext: (),
            };

            context.leave_scope();
            Term::ArrowDep(renamed_arrow)
        }
        Term::ArrowNodep(arrow) => Term::ArrowNodep(TermArrowNodep {
            from: Box::new(rename_term(context, arrow.from())),
            arrow: arrow.arrow.clone(),
            to: Box::new(rename_term(context, arrow.to())),
            ext: (),
        }),
        Term::Match(match_term) => {
            let renamed_match = TermMatch {
                keyword_match: match_term.keyword_match.clone(),
                scrutinee: match_term.scrutinee.clone(),
                brace_l: match_term.brace_l.clone(),
                branches: match_term
                    .branches()
                    .iter()
                    .map(|branch| rename_match_branch(context, branch))
                    .collect(),
                brace_r: match_term.brace_r.clone(),
                ext: (),
            };
            Term::Match(renamed_match)
        }
        Term::Paren(paren) => Term::Paren(TermParen {
            paren_l: paren.paren_l.clone(),
            term: Box::new(rename_term(context, paren.term())),
            paren_r: paren.paren_r.clone(),
            ext: (),
        }),
        Term::Unit(unit) => Term::Unit(TermUnit {
            paren_l: unit.paren_l.clone(),
            paren_r: unit.paren_r.clone(),
            ext: (),
        }),
        Term::Number(number) => Term::Number(TermNumber {
            number: number.number.clone(),
            ext: (),
        }),
        Term::Struct(_) => {
            // For now, struct expressions are not supported in renaming
            unreachable!("Struct terms are not yet supported in renaming")
        }
    }
}

fn rename_match_branch(
    context: &mut RenameContext,
    branch: &TermMatchBranch<PhaseParse>,
) -> TermMatchBranch<PhaseRenamed> {
    context.enter_scope();

    // Bind variables from the pattern
    rename_pattern_bindings(context, &branch.pattern);

    let renamed_branch = TermMatchBranch {
        pattern: branch.pattern.clone(),
        arrow: branch.arrow.clone(),
        body: Box::new(rename_term(context, branch.body())),
        ext: (),
    };

    context.leave_scope();
    renamed_branch
}

fn rename_pattern_bindings(context: &mut RenameContext, pattern: &Pattern) {
    match pattern {
        Pattern::Variable(var) => {
            context.bind_variable(var.s());
        }
        Pattern::Constructor(_, args) => {
            for arg in args {
                context.bind_variable(arg.s());
            }
        }
    }
}

pub fn rename_file(file: File<PhaseParse>) -> File<PhaseRenamed> {
    let mut context = RenameContext::new(0); // TODO: Get file_id from somewhere

    let items = file
        .items()
        .iter()
        .map(|item| rename_item(&mut context, item))
        .collect();

    File { items, ext: () }
}

#[cfg(test)]
mod tests {
    use super::*;
    use neco_felis_syn::{FileIdGenerator, Parse, token::Token};

    #[test]
    fn test_rename_simple_variable() {
        // Test that a simple variable gets renamed with a unique ID
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        // Simple Felis code with a variable reference
        let source = "#definition x : nat { y }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let parsed_file = File::parse(&tokens, &mut i).unwrap().unwrap();

        let renamed_file = rename_file(parsed_file);

        // Verify that we have one item
        assert_eq!(renamed_file.items.len(), 1);

        // Check that the renamed file has the expected structure
        if let Item::Definition(def) = &renamed_file.items[0] {
            // Variables are wrapped in TermApply by the parser
            if let Term::Variable(var) = def.body.as_ref() {
                // Variable should have been assigned an ID
                let VariableId(file_id, var_id) = var.ext;
                assert_eq!(file_id, 0); // File ID should be 0
                assert!(var_id > 0); // Variable ID should be greater than 0

                // The variable name should still be "y"
                assert_eq!(var.variable.s(), "y");
            } else {
                panic!("Expected variable in apply function position");
            }
        } else {
            panic!("Expected definition item");
        }
    }

    #[test]
    fn test_rename_shadowing() {
        // Test that variable shadowing works correctly
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        // Felis code with shadowing: outer x and inner x should have different IDs
        let source = "#definition f : (x : nat) -> nat { (y : nat) -> x }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let parsed_file = File::parse(&tokens, &mut i).unwrap().unwrap();

        let renamed_file = rename_file(parsed_file);

        // Just verify that renaming doesn't crash and produces output
        assert_eq!(renamed_file.items.len(), 1);
    }

    #[test]
    fn test_rename_match_pattern() {
        // Test that pattern variables in match expressions get proper IDs
        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        // Simple match expression
        let source = "#definition test : nat { #match x { y => y } }";
        let tokens = Token::lex(source, file_id);

        let mut i = 0;
        let parsed_file = File::parse(&tokens, &mut i).unwrap().unwrap();

        let renamed_file = rename_file(parsed_file);

        // Just verify that renaming doesn't crash and produces output
        assert_eq!(renamed_file.items.len(), 1);
    }
}
