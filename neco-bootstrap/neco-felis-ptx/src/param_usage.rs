use neco_felis_syn::{PhaseParse, ProcTerm, Statement, Statements};
use std::collections::{HashMap, HashSet};

fn add_param_field_usage(usage: &mut HashMap<String, Vec<String>>, object: &str, field: &str) {
    let entry = usage.entry(object.to_string()).or_default();
    if !entry.iter().any(|existing| existing == field) {
        entry.push(field.to_string());
    }
}

fn collect_param_fields_from_proc_term(
    proc_term: &ProcTerm<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match proc_term {
        ProcTerm::FieldAccess(field_access) => {
            let object = field_access.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, field_access.field_name());
            }
        }
        ProcTerm::MethodChain(method_chain) => {
            let object = method_chain.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, method_chain.field_name());
            }
            if let Some(index) = &method_chain.index {
                collect_param_fields_from_proc_term(index, param_set, usage);
            }
        }
        ProcTerm::Apply(apply) => {
            collect_param_fields_from_proc_term(&apply.f, param_set, usage);
            for arg in &apply.args {
                collect_param_fields_from_proc_term(arg, param_set, usage);
            }
        }
        ProcTerm::StructValue(struct_value) => {
            for field in &struct_value.fields {
                collect_param_fields_from_proc_term(&field.value, param_set, usage);
            }
        }
        ProcTerm::Paren(paren) => {
            collect_param_fields_from_proc_term(&paren.proc_term, param_set, usage);
        }
        ProcTerm::If(proc_if) => {
            collect_param_fields_from_statements(&proc_if.condition, param_set, usage);
            collect_param_fields_from_statements(&proc_if.then_body, param_set, usage);
            if let Some(else_clause) = &proc_if.else_clause {
                collect_param_fields_from_statements(&else_clause.else_body, param_set, usage);
            }
        }
        ProcTerm::ConstructorCall(constructor_call) => {
            for arg in &constructor_call.args {
                collect_param_fields_from_proc_term(arg, param_set, usage);
            }
        }
        ProcTerm::Dereference(deref) => {
            collect_param_fields_from_proc_term(&deref.term, param_set, usage);
        }
        ProcTerm::Ext(_)
        | ProcTerm::Variable(_)
        | ProcTerm::Unit(_)
        | ProcTerm::Number(_)
        | ProcTerm::Struct(_) => {}
    }
}

fn collect_param_fields_from_statement(
    statement: &Statement<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match statement {
        Statement::FieldAssign(assign) => {
            let object = assign.method_chain.object_name();
            if param_set.contains(object) {
                add_param_field_usage(usage, object, assign.method_chain.field_name());
            }
            if let Some(index) = &assign.method_chain.index {
                collect_param_fields_from_proc_term(index, param_set, usage);
            }
            collect_param_fields_from_proc_term(&assign.value, param_set, usage);
        }
        Statement::Let(let_stmt) => {
            collect_param_fields_from_proc_term(&let_stmt.value, param_set, usage);
        }
        Statement::LetMut(let_mut_stmt) => {
            collect_param_fields_from_proc_term(&let_mut_stmt.value, param_set, usage);
        }
        Statement::Assign(assign_stmt) => {
            collect_param_fields_from_proc_term(&assign_stmt.value, param_set, usage);
        }
        Statement::Loop(loop_stmt) => {
            collect_param_fields_from_statements(&loop_stmt.body, param_set, usage);
        }
        Statement::Return(return_stmt) => {
            collect_param_fields_from_proc_term(&return_stmt.value, param_set, usage);
        }
        Statement::Expr(proc_term) => {
            collect_param_fields_from_proc_term(proc_term, param_set, usage);
        }
        Statement::CallPtx(_) | Statement::Break(_) | Statement::Ext(_) => {}
    }
}

fn collect_param_fields_from_statements(
    statements: &Statements<PhaseParse>,
    param_set: &HashSet<&str>,
    usage: &mut HashMap<String, Vec<String>>,
) {
    match statements {
        Statements::Then(then) => {
            collect_param_fields_from_statement(&then.head, param_set, usage);
            collect_param_fields_from_statements(&then.tail, param_set, usage);
        }
        Statements::Statement(statement) => {
            collect_param_fields_from_statement(statement, param_set, usage);
        }
        Statements::Nil => {}
    }
}

pub(crate) fn collect_param_field_usage(
    statements: &Statements<PhaseParse>,
    param_names: &[String],
) -> HashMap<String, Vec<String>> {
    if param_names.is_empty() {
        return HashMap::new();
    }
    let param_set: HashSet<&str> = param_names.iter().map(|s| s.as_str()).collect();
    let mut usage: HashMap<String, Vec<String>> = HashMap::new();
    collect_param_fields_from_statements(statements, &param_set, &mut usage);
    usage
}

#[cfg(test)]
mod tests {
    use super::*;
    use neco_felis_syn::{File, FileIdGenerator, Item, Parse, Token};
    use std::collections::HashSet as StdHashSet;
    use std::path::PathBuf;

    #[test]
    fn collect_param_field_usage_extracts_unique_fields() {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let sample_path = manifest_dir.join("../../testcases/felis/single/ptx_proc_call.fe");
        let source =
            std::fs::read_to_string(sample_path).expect("read sample felis file for PTX test");

        let mut file_id_generator = FileIdGenerator::new();
        let file_id = file_id_generator.generate_file_id();

        let tokens = Token::lex(&source, file_id);
        let mut idx = 0;
        let file = File::parse(&tokens, &mut idx)
            .expect("parse file")
            .expect("ast from file");
        assert_eq!(idx, tokens.len(), "parser should consume all tokens");

        let ptx_proc = file
            .items()
            .iter()
            .find_map(|item| match item {
                Item::Proc(proc_item)
                    if proc_item.ptx_modifier.is_some() && proc_item.name.s() == "f" =>
                {
                    Some(proc_item)
                }
                _ => None,
            })
            .expect("locate #ptx proc f");

        let params = vec!["ps".to_string()];
        let usage = collect_param_field_usage(&ptx_proc.proc_block.statements, &params);

        assert_eq!(
            usage.len(),
            1,
            "only ps parameter should have tracked fields"
        );

        let fields = usage.get("ps").expect("fields for ps");
        assert_eq!(fields.len(), 3, "expected unique field entries for ps");

        let mut sorted_fields = fields.clone();
        sorted_fields.sort();
        assert_eq!(
            sorted_fields,
            vec!["b".to_string(), "g".to_string(), "r".to_string()]
        );

        let unique: StdHashSet<_> = fields.iter().collect();
        assert_eq!(
            unique.len(),
            fields.len(),
            "duplicate field names should be eliminated"
        );
    }
}
