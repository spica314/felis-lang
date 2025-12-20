// Position and file handling
pub mod position {
    pub mod file;
    pub mod file_id;
    pub mod pos;
}

// Items (top-level language constructs)
pub mod items {
    pub mod item;
    pub mod item_definition;
    pub mod item_entrypoint;
    pub mod item_inductive;
    pub mod item_inductive_branch;
    pub mod item_proc;
    pub mod item_proc_block;
    pub mod item_submodule;
    pub mod item_theorem;
    pub mod item_type;
    pub mod item_use;
    pub mod item_use_builtin;
}

// Terms (expression syntax)
pub mod terms {
    pub mod term;
    pub mod term_apply;
    pub mod term_arrow_dep;
    pub mod term_arrow_nodep;
    pub mod term_match;
    pub mod term_match_branch;
    pub mod term_number;
    pub mod term_paren;
    pub mod term_struct;
    pub mod term_unit;
    pub mod term_variable;
}

// Procedural terms (procedural expression syntax)
pub mod proc_terms {
    pub mod proc_term;
    pub mod proc_term_apply;
    pub mod proc_term_constructor_call;
    pub mod proc_term_field_access;
    pub mod proc_term_if;
    pub mod proc_term_match;
    pub mod proc_term_method_chain;
    pub mod proc_term_number;
    pub mod proc_term_paren;
    pub mod proc_term_struct_value;
    pub mod proc_term_unit;
    pub mod proc_term_variable;
}

// Statements (statement syntax)
pub mod statements {
    pub mod statement;
    pub mod statement_assign;
    pub mod statement_break;
    pub mod statement_let;
    pub mod statement_let_mut;
    pub mod statement_loop;
    pub mod statement_method_chain;
    pub mod statement_return;
    pub mod statements_list;
    pub mod statements_then;
}

// Parsing infrastructure
pub mod parsing {
    pub mod parse;
    pub mod phase;
    pub mod token;
}

// Re-export all modules for backward compatibility
pub use position::file::*;
pub use position::file_id::*;
pub use position::pos::*;

pub use items::item::*;
pub use items::item_definition::*;
pub use items::item_entrypoint::*;
pub use items::item_inductive::*;
pub use items::item_inductive_branch::*;
pub use items::item_proc::*;
pub use items::item_proc_block::*;
pub use items::item_submodule::*;
pub use items::item_theorem::*;
pub use items::item_type::*;
pub use items::item_use::*;
pub use items::item_use_builtin::*;

pub use terms::term::*;
pub use terms::term_apply::*;
pub use terms::term_arrow_dep::*;
pub use terms::term_arrow_nodep::*;
pub use terms::term_match::*;
pub use terms::term_match_branch::*;
pub use terms::term_number::*;
pub use terms::term_paren::*;
pub use terms::term_struct::*;
pub use terms::term_unit::*;
pub use terms::term_variable::*;

pub use proc_terms::proc_term::*;
pub use proc_terms::proc_term_apply::*;
pub use proc_terms::proc_term_constructor_call::*;
pub use proc_terms::proc_term_field_access::*;
pub use proc_terms::proc_term_if::*;
pub use proc_terms::proc_term_match::*;
pub use proc_terms::proc_term_method_chain::*;
pub use proc_terms::proc_term_number::*;
pub use proc_terms::proc_term_paren::*;
pub use proc_terms::proc_term_struct_value::*;
pub use proc_terms::proc_term_unit::*;
pub use proc_terms::proc_term_variable::*;

pub use statements::statement::*;
pub use statements::statement_assign::*;
pub use statements::statement_break::*;
pub use statements::statement_let::*;
pub use statements::statement_let_mut::*;
pub use statements::statement_loop::*;
pub use statements::statement_method_chain::*;
pub use statements::statement_return::*;
pub use statements::statements_list::*;
pub use statements::statements_then::*;

pub use parsing::parse::*;
pub use parsing::phase::*;
pub use parsing::token::*;

// Re-export token module for backward compatibility with crate::token:: paths
pub use parsing::token;
