use std::collections::{HashMap, HashSet};

use neco_rs_parser::{
    ArrowParameter, BindingPattern, ElseBranch, Item, LetOperator, ParsedPackage, Statement, Term,
};

use crate::effect::Value;
use crate::ir::{ArrayElementType, CompiledPtxArtifact, LoweredProgram, intern_data};
use crate::{Error, Result};

use super::declarations::StructSignature;
use super::scalar::{ScalarBinaryOp, ScalarPrimitive, ScalarType, ScalarUnaryOp, scalar_primitive};
use super::symbol::SymbolTable;
use super::{LoweringState, normalize_numeric_literal_arguments};

pub(super) fn initialize_compile_ptx_bindings(
    package: &ParsedPackage,
    symbols: &SymbolTable<'_>,
    ptx_functions: &HashMap<String, PtxFunction>,
    state: &mut LoweringState,
    program: &mut LoweredProgram,
) -> Result<()> {
    for item in package
        .source_files
        .iter()
        .flat_map(|file| file.syntax.items.iter())
    {
        let Item::CompilePtx(compile_ptx) = item else {
            continue;
        };
        let function = symbols
            .find_function_in_package(&package.manifest.name, &compile_ptx.function_name)
            .ok_or_else(|| {
                Error::Unsupported(format!(
                    "`#compile_ptx` target `{}` was not found",
                    compile_ptx.function_name
                ))
            })?;
        let ptx = compile_empty_ptx_function(function, &state.structs, ptx_functions)?;
        let len = i32::try_from(ptx.len()).map_err(|_| {
            Error::Unsupported(format!(
                "compiled PTX for `{}` is too large",
                compile_ptx.function_name
            ))
        })?;
        let data_index = intern_data(program, ptx);
        program.compiled_ptx.push(CompiledPtxArtifact {
            data_index,
            function_name: function.name.name.clone(),
        });
        let previous = state.environment.insert(
            compile_ptx.value_name.clone(),
            Value::StaticSlice { data_index, len },
        );
        if previous.is_some() {
            return Err(Error::Unsupported(format!(
                "duplicate compiled PTX value `{}`",
                compile_ptx.value_name
            )));
        }
    }
    Ok(())
}

fn compile_empty_ptx_function(
    function: &neco_rs_parser::FunctionDeclaration,
    structs: &HashMap<String, StructSignature>,
    ptx_functions: &HashMap<String, PtxFunction>,
) -> Result<Vec<u8>> {
    if function
        .effect
        .as_ref()
        .is_none_or(|effect| effect.lexeme != "PTX")
    {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` target `{}` must use `#with PTX`",
            function.name.name
        )));
    }
    validate_empty_ptx_signature(function)?;
    if !matches!(function.body.tail.as_deref(), Some(Term::Unit)) {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only PTX function bodies ending in `()` for `{}`",
            function.name.name
        )));
    }

    let body = compile_ptx_body(function, structs, ptx_functions)?;
    let u64_regs = if body.next_u64_register == 1 {
        String::new()
    } else {
        format!("    .reg .u64 %rd<{}>;\n", body.next_u64_register)
    };
    let u32_regs = if body.next_u32_register == 1 {
        String::new()
    } else {
        format!("    .reg .u32 %r<{}>;\n", body.next_u32_register)
    };
    let f32_regs = if body.next_f32_register == 1 {
        String::new()
    } else {
        format!("    .reg .f32 %f<{}>;\n", body.next_f32_register)
    };
    let pred_regs = if body.next_pred_register == 1 {
        String::new()
    } else {
        format!("    .reg .pred %p<{}>;\n", body.next_pred_register)
    };
    let parameter_load = if body.loads_array_parameter {
        "    ld.param.u64 %rd1, [arg0];\n"
    } else {
        ""
    };
    let mut ptx = format!(
        ".version 7.0\n.target sm_52\n.address_size 64\n\n.visible .entry {}(\n    .param {} arg0\n)\n{{\n{}{}{}{}{}{}    ret;\n}}\n",
        function.name.name,
        ptx_parameter_type(function)?,
        u64_regs,
        u32_regs,
        f32_regs,
        pred_regs,
        parameter_load,
        body.instructions
    )
    .into_bytes();
    ptx.push(0);
    Ok(ptx)
}

struct CompiledPtxBody {
    instructions: String,
    next_u32_register: usize,
    next_u64_register: usize,
    next_f32_register: usize,
    next_pred_register: usize,
    loads_array_parameter: bool,
}

#[derive(Clone)]
pub(super) struct PtxFunction {
    parameters: Vec<PtxFunctionParameter>,
    result_ty: Term,
    body: neco_rs_parser::Block,
}

#[derive(Clone)]
struct PtxFunctionParameter {
    name: String,
    ty: Term,
}

pub(super) fn collect_ptx_functions(
    symbols: &SymbolTable<'_>,
) -> Result<HashMap<String, PtxFunction>> {
    let mut functions = HashMap::new();
    for function in symbols.function_declarations() {
        if function
            .effect
            .as_ref()
            .is_none_or(|effect| effect.lexeme != "PTX")
        {
            continue;
        }
        let name = function.name.name.clone();
        let value = ptx_function_from_decl(function)?;
        if functions.insert(name.clone(), value).is_some() {
            return Err(Error::Unsupported(format!(
                "duplicate function `{name}` is not supported"
            )));
        }
    }
    Ok(functions)
}

fn ptx_function_from_decl(function: &neco_rs_parser::FunctionDeclaration) -> Result<PtxFunction> {
    let mut parameter_names = HashSet::new();
    let mut parameters = Vec::new();
    let mut current = &function.ty;
    while let Term::Arrow(arrow) = current {
        let ArrowParameter::Binder(binder) = &arrow.parameter else {
            return Err(Error::Unsupported(format!(
                "PTX function `{}` must use named parameters",
                function.name.name
            )));
        };
        if !parameter_names.insert(binder.name.clone()) {
            return Err(Error::Unsupported(format!(
                "duplicate parameter `{}` in PTX function `{}`",
                binder.name, function.name.name
            )));
        }
        parameters.push(PtxFunctionParameter {
            name: binder.name.clone(),
            ty: binder.ty.as_ref().clone(),
        });
        current = arrow.result.as_ref();
    }
    if function.body.tail.is_none() {
        return Err(Error::Unsupported(format!(
            "PTX function `{}` body must end with a value expression",
            function.name.name
        )));
    }
    Ok(PtxFunction {
        parameters,
        result_ty: current.clone(),
        body: function.body.clone(),
    })
}

fn compile_ptx_body(
    function: &neco_rs_parser::FunctionDeclaration,
    structs: &HashMap<String, StructSignature>,
    ptx_functions: &HashMap<String, PtxFunction>,
) -> Result<CompiledPtxBody> {
    if function.body.statements.is_empty() {
        return Ok(CompiledPtxBody {
            instructions: String::new(),
            next_u32_register: 1,
            next_u64_register: 1,
            next_f32_register: 1,
            next_pred_register: 1,
            loads_array_parameter: false,
        });
    }
    let parameter = ptx_parameter_name(function)?;
    let element_type = ptx_array_parameter_element_type(function)?;
    let mut compiler = PtxBodyCompiler {
        function_name: &function.name.name,
        parameter,
        element_type,
        structs,
        ptx_functions,
        locals: HashMap::new(),
        local_refs: HashMap::new(),
        array_aliases: HashMap::new(),
        loop_stack: Vec::new(),
        instructions: String::new(),
        next_u32_register: 1,
        next_u64_register: 2,
        next_f32_register: 1,
        next_pred_register: 1,
        next_label: 0,
        inline_stack: vec![function.name.name.clone()],
    };
    for statement in &function.body.statements {
        compiler.compile_statement(statement)?;
    }
    Ok(CompiledPtxBody {
        instructions: compiler.instructions,
        next_u32_register: compiler.next_u32_register,
        next_u64_register: compiler.next_u64_register,
        next_f32_register: compiler.next_f32_register,
        next_pred_register: compiler.next_pred_register,
        loads_array_parameter: true,
    })
}

#[derive(Clone)]
enum PtxValue {
    Scalar(PtxScalarValue),
    Struct(PtxStructValue),
    Unit,
}

#[derive(Clone)]
struct PtxScalarValue {
    ty: ArrayElementType,
    register: String,
}

#[derive(Clone)]
struct PtxStructValue {
    type_name: String,
    fields: Vec<PtxStructFieldValue>,
}

#[derive(Clone)]
struct PtxStructFieldValue {
    name: String,
    value: PtxValue,
}

struct PtxBodyCompiler<'a> {
    function_name: &'a str,
    parameter: &'a str,
    element_type: ArrayElementType,
    structs: &'a HashMap<String, StructSignature>,
    ptx_functions: &'a HashMap<String, PtxFunction>,
    locals: HashMap<String, PtxValue>,
    local_refs: HashMap<String, String>,
    array_aliases: HashMap<String, String>,
    loop_stack: Vec<(String, String)>,
    instructions: String,
    next_u32_register: usize,
    next_u64_register: usize,
    next_f32_register: usize,
    next_pred_register: usize,
    next_label: usize,
    inline_stack: Vec<String>,
}

impl PtxBodyCompiler<'_> {
    fn compile_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Let(let_statement)
                if matches!(let_statement.operator, LetOperator::Equals) =>
            {
                let BindingPattern::Name(name) = &let_statement.binder else {
                    return Err(self.unsupported("PTX `#let` currently requires a named binder"));
                };
                let value = self.compile_value(&let_statement.value)?;
                self.locals.insert(name.clone(), value);
                Ok(())
            }
            Statement::LetRef(letref_statement) => {
                let Some(source_name) = simple_path_name(letref_statement.source.as_ref()) else {
                    return Err(
                        self.unsupported("PTX `#letref` currently requires a simple source local")
                    );
                };
                let Some(PtxValue::Scalar(_)) = self.locals.get(source_name) else {
                    return Err(self.unsupported("PTX `#letref` source must be a scalar local"));
                };
                self.local_refs
                    .insert(letref_statement.reference.clone(), source_name.to_string());
                Ok(())
            }
            Statement::If(if_stmt) => self.compile_if_statement(if_stmt),
            Statement::Loop(loop_stmt) => self.compile_loop_statement(loop_stmt),
            Statement::Break => {
                let Some((_, break_label)) = self.loop_stack.last() else {
                    return Err(self.unsupported("PTX `#break` is only supported inside `#loop`"));
                };
                self.instructions.push_str(&format!("    bra {};\n", break_label));
                Ok(())
            }
            Statement::Continue => {
                let Some((continue_label, _)) = self.loop_stack.last() else {
                    return Err(
                        self.unsupported("PTX `#continue` is only supported inside `#loop`")
                    );
                };
                self.instructions
                    .push_str(&format!("    bra {};\n", continue_label));
                Ok(())
            }
            Statement::Expression(term) => {
                if let Some((ref_name, value)) = ptx_local_ref_set_call_parts(term)? {
                    self.compile_local_ref_set(&ref_name, &value)?;
                    return Ok(());
                }
                if let Some((array_name, index, value)) = ptx_ref_set_call_parts(term)? {
                    self.ensure_parameter_array(&array_name)?;
                    let address = self.compile_array_address(&index)?;
                    let value = self.compile_scalar_value(&value)?;
                    self.ensure_element_type(value.ty)?;
                    self.instructions.push_str(&format!(
                        "    st.global.{} [{}], {};\n",
                        ptx_memory_type(value.ty),
                        address,
                        value.register
                    ));
                    return Ok(());
                }
                if self.compile_ptx_unit_call(term)? {
                    return Ok(());
                }
                Err(self.unsupported(
                    "PTX expression statements currently support only `ref_set_ptx` or unit-returning PTX function calls",
                ))
            }
            _ => Err(self.unsupported(
                "PTX bodies currently support only `#let`, `#letref`, `#if`, `#loop`, loop control, and ref expression statements",
            )),
        }
    }

    fn compile_local_ref_set(&mut self, ref_name: &str, value: &Term) -> Result<()> {
        let Some(local_name) = self.local_refs.get(ref_name).cloned() else {
            return Err(self.unsupported("PTX `ref_set` must target a local scalar reference"));
        };
        let Some(PtxValue::Scalar(target)) = self.locals.get(&local_name).cloned() else {
            return Err(self.unsupported("PTX `ref_set` target must be a scalar local"));
        };
        let value = self.compile_scalar_value(value)?;
        if value.ty != target.ty {
            return Err(self.unsupported("PTX `ref_set` value type must match its target"));
        }
        self.instructions.push_str(&format!(
            "    mov.{} {}, {};\n",
            ptx_register_type(target.ty),
            target.register,
            value.register
        ));
        Ok(())
    }

    fn compile_if_statement(&mut self, if_stmt: &neco_rs_parser::IfStatement) -> Result<()> {
        let predicate = self.compile_condition(if_stmt.condition.as_ref())?;
        let else_label = self.allocate_label("else");
        let end_label = self.allocate_label("endif");
        self.instructions
            .push_str(&format!("    @!{} bra {};\n", predicate, else_label));

        self.compile_scoped_statements(&if_stmt.then_block.statements)?;
        self.instructions
            .push_str(&format!("    bra {};\n", end_label));
        self.instructions.push_str(&format!("{}:\n", else_label));
        if let Some(else_branch) = &if_stmt.else_branch {
            match else_branch {
                ElseBranch::Block(block) => self.compile_scoped_statements(&block.statements)?,
                ElseBranch::If(else_if) => self.compile_if_statement(else_if)?,
            }
        }
        self.instructions.push_str(&format!("{}:\n", end_label));
        Ok(())
    }

    fn compile_loop_statement(&mut self, loop_stmt: &neco_rs_parser::LoopStatement) -> Result<()> {
        let start_label = self.allocate_label("loop");
        let end_label = self.allocate_label("endloop");
        self.instructions.push_str(&format!("{}:\n", start_label));
        self.loop_stack
            .push((start_label.clone(), end_label.clone()));
        self.compile_scoped_statements(&loop_stmt.body.statements)?;
        self.loop_stack.pop();
        self.instructions
            .push_str(&format!("    bra {};\n", start_label));
        self.instructions.push_str(&format!("{}:\n", end_label));
        Ok(())
    }

    fn compile_scoped_statements(&mut self, statements: &[Statement]) -> Result<()> {
        let saved_locals = self.locals.clone();
        let saved_local_refs = self.local_refs.clone();
        for statement in statements {
            self.compile_statement(statement)?;
        }
        self.locals = saved_locals;
        self.local_refs = saved_local_refs;
        Ok(())
    }

    fn compile_condition(&mut self, term: &Term) -> Result<String> {
        let Some((callee, arguments)) = flatten_application(term) else {
            return Err(self.unsupported("PTX `#if` condition must be a comparison"));
        };
        let Some(primitive) = simple_path_name(callee) else {
            return Err(self.unsupported("PTX `#if` condition must use a simple comparison"));
        };
        let Some((ty, comparison)) = ptx_comparison_primitive(primitive) else {
            return Err(self.unsupported("PTX `#if` condition must be a supported comparison"));
        };
        let normalized = normalize_numeric_literal_arguments(&arguments);
        let [lhs, rhs] = normalized.as_slice() else {
            return Err(self.unsupported("PTX comparison must receive exactly two arguments"));
        };
        let lhs = self.compile_scalar_value(lhs)?;
        let rhs = self.compile_scalar_value(rhs)?;
        if lhs.ty != ty || rhs.ty != ty {
            return Err(self.unsupported("PTX comparison arguments must match its type"));
        }
        let predicate = self.allocate_predicate_register();
        self.instructions.push_str(&format!(
            "    setp.{}.{} {}, {}, {};\n",
            comparison,
            ptx_comparison_type(ty),
            predicate,
            lhs.register,
            rhs.register
        ));
        Ok(predicate)
    }

    fn compile_value(&mut self, term: &Term) -> Result<PtxValue> {
        if let Term::Group(inner) = term {
            return self.compile_value(inner);
        }

        if let Some(ref_name) = ptx_local_ref_get_call_parts(term)? {
            let Some(local_name) = self.local_refs.get(&ref_name) else {
                return Err(self.unsupported("PTX `ref_get` must target a local scalar reference"));
            };
            let Some(value) = self.locals.get(local_name).cloned() else {
                return Err(self.unsupported("PTX `ref_get` target must be a scalar local"));
            };
            return Ok(value);
        }

        if let Some((array_name, index)) = ptx_ref_get_call_parts(term)? {
            self.ensure_parameter_array(&array_name)?;
            let address = self.compile_array_address(&index)?;
            let register = self.allocate_register(self.element_type);
            self.instructions.push_str(&format!(
                "    ld.global.{} {}, [{}];\n",
                ptx_memory_type(self.element_type),
                register,
                address
            ));
            return Ok(PtxValue::Scalar(PtxScalarValue {
                ty: self.element_type,
                register,
            }));
        }

        if let Some(value) = self.compile_literal(term)? {
            return Ok(PtxValue::Scalar(value));
        }

        if let Some(value) = self.compile_primitive_call(term)? {
            return Ok(PtxValue::Scalar(value));
        }

        if let Some(value) = self.compile_ptx_value_call(term)? {
            return Ok(value);
        }

        if let Some(value) = self.compile_struct_literal(term)? {
            return Ok(PtxValue::Struct(value));
        }

        if let Some(value) = self.compile_field_access(term)? {
            return Ok(value);
        }

        if let Some(value) = self.compile_special_register(term) {
            return Ok(PtxValue::Scalar(value));
        }

        let Some(name) = simple_path_name(term) else {
            return Err(self.unsupported(
                "PTX value must be a literal, local, primitive call, special register, or ref_get_ptx",
            ));
        };
        self.locals
            .get(name)
            .cloned()
            .ok_or_else(|| self.unsupported("PTX value must refer to a previously bound local"))
    }

    fn compile_scalar_value(&mut self, term: &Term) -> Result<PtxScalarValue> {
        match self.compile_value(term)? {
            PtxValue::Scalar(value) => Ok(value),
            PtxValue::Struct(_) => Err(self.unsupported("PTX expression requires a scalar value")),
            PtxValue::Unit => Err(self.unsupported("PTX expression requires a scalar value")),
        }
    }

    fn compile_array_address(&mut self, index: &Term) -> Result<String> {
        if let Some(index) = parse_ptx_i32_literal(index)? {
            return Ok(format!(
                "%rd1+{}",
                index * ptx_element_size(self.element_type)
            ));
        }
        let index = self.compile_scalar_value(index)?;
        if index.ty != ArrayElementType::I32 {
            return Err(self.unsupported("PTX array index must be an i32 value"));
        }
        let offset = self.allocate_u64_register();
        let address = self.allocate_u64_register();
        self.instructions.push_str(&format!(
            "    mul.wide.s32 {}, {}, {};\n",
            offset,
            index.register,
            ptx_element_size(self.element_type)
        ));
        self.instructions
            .push_str(&format!("    add.s64 {}, %rd1, {};\n", address, offset));
        Ok(address)
    }

    fn compile_literal(&mut self, term: &Term) -> Result<Option<PtxScalarValue>> {
        if let Some(value) = parse_ptx_i32_literal(term)? {
            let register = self.allocate_u32_register();
            self.instructions
                .push_str(&format!("    mov.u32 {}, {};\n", register, value));
            return Ok(Some(PtxScalarValue {
                ty: ArrayElementType::I32,
                register,
            }));
        }
        if let Some(value) = parse_ptx_i64_literal(term)? {
            let register = self.allocate_u64_register();
            self.instructions
                .push_str(&format!("    mov.u64 {}, {};\n", register, value));
            return Ok(Some(PtxScalarValue {
                ty: ArrayElementType::I64,
                register,
            }));
        }
        if let Some(bits) = parse_ptx_f32_literal_bits(term)? {
            let register = self.allocate_f32_register();
            self.instructions
                .push_str(&format!("    mov.b32 {}, 0f{bits:08x};\n", register));
            return Ok(Some(PtxScalarValue {
                ty: ArrayElementType::F32,
                register,
            }));
        }
        Ok(None)
    }

    fn compile_primitive_call(&mut self, term: &Term) -> Result<Option<PtxScalarValue>> {
        let Some((callee, arguments)) = flatten_application(term) else {
            return Ok(None);
        };
        let Some(primitive) = simple_path_name(callee) else {
            return Ok(None);
        };
        if let Some(value) = self.compile_ptx_conversion_call(primitive, &arguments)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.compile_ptx_unary_call(primitive, &arguments)? {
            return Ok(Some(value));
        }
        let Some((ty, instruction)) = ptx_binary_primitive(primitive) else {
            return Ok(None);
        };
        let normalized = normalize_numeric_literal_arguments(&arguments);
        let [lhs, rhs] = normalized.as_slice() else {
            return Err(
                self.unsupported("PTX arithmetic primitive must receive exactly two arguments")
            );
        };
        let lhs = self.compile_scalar_value(lhs)?;
        let rhs = self.compile_scalar_value(rhs)?;
        if lhs.ty != ty || rhs.ty != ty {
            return Err(self.unsupported("PTX arithmetic primitive arguments must match its type"));
        }
        let register = self.allocate_register(ty);
        self.instructions.push_str(&format!(
            "    {instruction} {}, {}, {};\n",
            register, lhs.register, rhs.register
        ));
        Ok(Some(PtxScalarValue { ty, register }))
    }

    fn compile_ptx_conversion_call(
        &mut self,
        primitive: &str,
        arguments: &[Term],
    ) -> Result<Option<PtxScalarValue>> {
        let Some((source_ty, dest_ty, instruction)) = ptx_conversion_primitive(primitive) else {
            return Ok(None);
        };
        let normalized = normalize_numeric_literal_arguments(arguments);
        let [value] = normalized.as_slice() else {
            return Err(
                self.unsupported("PTX conversion primitive must receive exactly one argument")
            );
        };
        let value = self.compile_scalar_value(value)?;
        if value.ty != source_ty {
            return Err(self.unsupported("PTX conversion primitive argument has the wrong type"));
        }
        let register = self.allocate_register(dest_ty);
        self.instructions.push_str(&format!(
            "    {instruction} {}, {};\n",
            register, value.register
        ));
        Ok(Some(PtxScalarValue {
            ty: dest_ty,
            register,
        }))
    }

    fn compile_ptx_unary_call(
        &mut self,
        primitive: &str,
        arguments: &[Term],
    ) -> Result<Option<PtxScalarValue>> {
        let Some((ty, instruction)) = ptx_unary_primitive(primitive) else {
            return Ok(None);
        };
        let normalized = normalize_numeric_literal_arguments(arguments);
        let [value] = normalized.as_slice() else {
            return Err(self.unsupported("PTX unary primitive must receive exactly one argument"));
        };
        let value = self.compile_scalar_value(value)?;
        if value.ty != ty {
            return Err(self.unsupported("PTX unary primitive argument has the wrong type"));
        }
        let register = self.allocate_register(ty);
        self.instructions.push_str(&format!(
            "    {instruction} {}, {};\n",
            register, value.register
        ));
        Ok(Some(PtxScalarValue { ty, register }))
    }

    fn compile_ptx_value_call(&mut self, term: &Term) -> Result<Option<PtxValue>> {
        let Some((function_name, function, arguments)) = self.ptx_call_parts(term)? else {
            return Ok(None);
        };
        if matches!(function.result_ty, Term::Unit) {
            return Err(self.unsupported("PTX value call must return a value"));
        }
        let result = self.inline_ptx_call(function_name, function.clone(), &arguments)?;
        self.validate_value_against_type(&result, &function.result_ty)?;
        Ok(Some(result))
    }

    fn compile_ptx_unit_call(&mut self, term: &Term) -> Result<bool> {
        let Some((function_name, function, arguments)) = self.ptx_call_parts(term)? else {
            return Ok(false);
        };
        if !matches!(function.result_ty, Term::Unit) {
            return Err(self.unsupported("PTX expression statement call must return `()`"));
        }
        let result = self.inline_ptx_call(function_name, function, &arguments)?;
        if !matches!(result, PtxValue::Unit) {
            return Err(self.unsupported("PTX expression statement call must return `()`"));
        }
        Ok(true)
    }

    fn ptx_call_parts(&self, term: &Term) -> Result<Option<(String, PtxFunction, Vec<Term>)>> {
        let Some((callee, arguments)) = flatten_application(term) else {
            return Ok(None);
        };
        let Some(function_name) = simple_path_name(callee) else {
            return Ok(None);
        };
        let Some(function) = self.ptx_functions.get(function_name) else {
            return Ok(None);
        };
        Ok(Some((
            function_name.to_string(),
            function.clone(),
            normalize_numeric_literal_arguments(&arguments),
        )))
    }

    fn inline_ptx_call(
        &mut self,
        function_name: String,
        function: PtxFunction,
        arguments: &[Term],
    ) -> Result<PtxValue> {
        if self.inline_stack.iter().any(|name| name == &function_name) {
            return Err(self.unsupported("recursive PTX function calls are not supported"));
        }
        if arguments.len() != function.parameters.len() {
            return Err(self.unsupported("PTX function call argument count does not match"));
        }

        let mut bound_scalars = Vec::new();
        let mut bound_arrays = Vec::new();
        for (parameter, argument) in function.parameters.iter().zip(arguments.iter()) {
            if let Some(element_type) = ptx_array_element_type(&parameter.ty)? {
                if element_type != self.element_type {
                    return Err(self.unsupported(
                        "PTX function ArrayVLPTX parameter element type must match the kernel parameter",
                    ));
                }
                let Some(argument_name) = simple_path_name(argument) else {
                    return Err(self.unsupported(
                        "PTX function ArrayVLPTX argument must be a simple array parameter",
                    ));
                };
                self.ensure_parameter_array(argument_name)?;
                bound_arrays.push((
                    parameter.name.clone(),
                    self.canonical_array_name(argument_name),
                ));
            } else {
                let value = self.compile_value(argument)?;
                self.validate_value_against_type(&value, &parameter.ty)?;
                bound_scalars.push((parameter.name.clone(), value));
            }
        }

        let saved_locals = self.locals.clone();
        let saved_local_refs = self.local_refs.clone();
        let saved_array_aliases = self.array_aliases.clone();
        self.inline_stack.push(function_name);
        for (name, value) in bound_scalars {
            self.locals.insert(name, value);
        }
        for (name, canonical) in bound_arrays {
            self.array_aliases.insert(name, canonical);
        }

        let result = (|| {
            for statement in &function.body.statements {
                self.compile_statement(statement)?;
            }
            let Some(tail) = function.body.tail.as_deref() else {
                return Err(self.unsupported("PTX function body must end with a value expression"));
            };
            if matches!(function.result_ty, Term::Unit) {
                if matches!(tail, Term::Unit) {
                    Ok(PtxValue::Unit)
                } else {
                    Err(self.unsupported("PTX unit function body must end with `()`"))
                }
            } else {
                self.compile_value(tail)
            }
        })();

        self.inline_stack.pop();
        self.locals = saved_locals;
        self.local_refs = saved_local_refs;
        self.array_aliases = saved_array_aliases;
        result
    }

    fn compile_struct_literal(&mut self, term: &Term) -> Result<Option<PtxStructValue>> {
        let Term::StructLiteral { path, fields } = term else {
            return Ok(None);
        };
        if path.token_keyword_package.is_some() || path.segments.len() != 1 {
            return Err(
                self.unsupported("PTX struct literal currently requires a simple type name")
            );
        }
        let type_name = &path.segments[0].lexeme;
        let Some(signature) = self.structs.get(type_name) else {
            return Err(self.unsupported("PTX struct literal refers to an unknown struct"));
        };
        if signature.is_rc {
            return Err(self.unsupported("PTX does not support `struct(rc)` values"));
        }

        let mut literal_fields = HashMap::new();
        for field in fields {
            if literal_fields
                .insert(field.name.as_str(), field.value.clone())
                .is_some()
            {
                return Err(self.unsupported("PTX struct literal contains a duplicate field"));
            }
        }

        let mut compiled_fields = Vec::with_capacity(signature.fields.len());
        for field in &signature.fields {
            let Some(value_term) = literal_fields.remove(field.name.as_str()) else {
                return Err(self.unsupported("PTX struct literal is missing a field"));
            };
            let value = self.compile_value(&value_term)?;
            self.validate_struct_field_value(&value, &field.ty)?;
            compiled_fields.push(PtxStructFieldValue {
                name: field.name.clone(),
                value,
            });
        }
        if !literal_fields.is_empty() {
            return Err(self.unsupported("PTX struct literal contains an unknown field"));
        }

        Ok(Some(PtxStructValue {
            type_name: type_name.clone(),
            fields: compiled_fields,
        }))
    }

    fn compile_field_access(&mut self, term: &Term) -> Result<Option<PtxValue>> {
        let Term::FieldAccess { receiver, field } = term else {
            return Ok(None);
        };
        let receiver = self.compile_value(receiver)?;
        let PtxValue::Struct(struct_value) = receiver else {
            return Err(self.unsupported("PTX field access requires a struct value"));
        };
        let Some(field_value) = struct_value
            .fields
            .iter()
            .find(|field_value| field_value.name == *field)
        else {
            return Err(self.unsupported("PTX field access refers to an unknown field"));
        };
        Ok(Some(field_value.value.clone()))
    }

    fn compile_special_register(&mut self, term: &Term) -> Option<PtxScalarValue> {
        let register_name = ptx_special_register(simple_path_name(term)?)?;
        let register = self.allocate_u32_register();
        self.instructions
            .push_str(&format!("    mov.u32 {}, {};\n", register, register_name));
        Some(PtxScalarValue {
            ty: ArrayElementType::I32,
            register,
        })
    }

    fn validate_struct_field_value(&self, value: &PtxValue, ty: &Term) -> Result<()> {
        self.validate_value_against_type(value, ty)
    }

    fn validate_value_against_type(&self, value: &PtxValue, ty: &Term) -> Result<()> {
        match (value, ptx_scalar_type(ty)) {
            (PtxValue::Scalar(value), Some(expected)) if value.ty == expected => return Ok(()),
            (PtxValue::Scalar(_), Some(_)) => {
                return Err(self.unsupported("PTX value does not match its type"));
            }
            _ => {}
        }

        if let (PtxValue::Struct(value), Some(expected)) = (value, simple_type_name(ty)) {
            let Some(signature) = self.structs.get(expected) else {
                return Err(self.unsupported("PTX struct field has an unknown struct type"));
            };
            if signature.is_rc {
                return Err(self.unsupported("PTX does not support `struct(rc)` values"));
            }
            if value.type_name == expected {
                return Ok(());
            }
            return Err(self.unsupported("PTX value does not match its type"));
        }

        Err(self.unsupported("PTX values currently support only scalar or struct types"))
    }

    fn allocate_u32_register(&mut self) -> String {
        let register = format!("%r{}", self.next_u32_register);
        self.next_u32_register += 1;
        register
    }

    fn allocate_u64_register(&mut self) -> String {
        let register = format!("%rd{}", self.next_u64_register);
        self.next_u64_register += 1;
        register
    }

    fn allocate_f32_register(&mut self) -> String {
        let register = format!("%f{}", self.next_f32_register);
        self.next_f32_register += 1;
        register
    }

    fn allocate_predicate_register(&mut self) -> String {
        let register = format!("%p{}", self.next_pred_register);
        self.next_pred_register += 1;
        register
    }

    fn allocate_label(&mut self, prefix: &str) -> String {
        let label = format!("${}_{}_{}", self.function_name, prefix, self.next_label);
        self.next_label += 1;
        label
    }

    fn allocate_register(&mut self, ty: ArrayElementType) -> String {
        match ty {
            ArrayElementType::I32 | ArrayElementType::U8 => self.allocate_u32_register(),
            ArrayElementType::I64 => self.allocate_u64_register(),
            ArrayElementType::F32 => self.allocate_f32_register(),
        }
    }

    fn ensure_parameter_array(&self, array_name: &str) -> Result<()> {
        if self.canonical_array_name(array_name) == self.parameter {
            Ok(())
        } else {
            Err(self.unsupported("PTX array access currently supports only the kernel parameter"))
        }
    }

    fn canonical_array_name(&self, array_name: &str) -> String {
        self.array_aliases
            .get(array_name)
            .cloned()
            .unwrap_or_else(|| array_name.to_string())
    }

    fn ensure_element_type(&self, ty: ArrayElementType) -> Result<()> {
        if self.element_type == ty {
            Ok(())
        } else {
            Err(self
                .unsupported("PTX ref_set_ptx value type must match the ArrayVLPTX element type"))
        }
    }

    fn unsupported(&self, message: &str) -> Error {
        Error::Unsupported(format!(
            "`#compile_ptx` for `{}`: {message}",
            self.function_name
        ))
    }
}

fn validate_empty_ptx_signature(function: &neco_rs_parser::FunctionDeclaration) -> Result<()> {
    let current = &function.ty;
    let Term::Arrow(arrow) = current else {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only one-argument PTX functions for `{}`",
            function.name.name
        )));
    };
    validate_ptx_parameter_type(&arrow.parameter, &function.name.name)?;
    let current = arrow.result.as_ref();
    if matches!(current, Term::Arrow(_)) {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only one-argument PTX functions for `{}`",
            function.name.name
        )));
    }
    if !matches!(current, Term::Unit) {
        return Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only PTX functions returning `()` for `{}`",
            function.name.name
        )));
    }
    Ok(())
}

fn ptx_parameter_type(function: &neco_rs_parser::FunctionDeclaration) -> Result<&'static str> {
    let Term::Arrow(arrow) = &function.ty else {
        unreachable!("validated PTX functions always have one parameter");
    };
    ptx_parameter_type_for_arrow_parameter(&arrow.parameter, &function.name.name)
}

fn validate_ptx_parameter_type(parameter: &ArrowParameter, function_name: &str) -> Result<()> {
    ptx_parameter_type_for_arrow_parameter(parameter, function_name).map(|_| ())
}

fn ptx_parameter_type_for_arrow_parameter(
    parameter: &ArrowParameter,
    function_name: &str,
) -> Result<&'static str> {
    let ty = match parameter {
        ArrowParameter::Binder(binder) => binder.ty.as_ref(),
        ArrowParameter::Domain(ty) => ty.as_ref(),
    };
    if ptx_array_element_type(ty)?.is_some() {
        return Ok(".u64");
    }
    match simple_type_name(ty) {
        Some("i32") => Ok(".u32"),
        Some("i64") => Ok(".u64"),
        Some("f32") => Ok(".f32"),
        Some("u8") => Ok(".u8"),
        _ => Err(Error::Unsupported(format!(
            "`#compile_ptx` currently supports only primitive i32/i64/f32/u8 parameters for `{function_name}`"
        ))),
    }
}

fn ptx_parameter_name(function: &neco_rs_parser::FunctionDeclaration) -> Result<&str> {
    let Term::Arrow(arrow) = &function.ty else {
        unreachable!("validated PTX functions always have one parameter");
    };
    match &arrow.parameter {
        ArrowParameter::Binder(binder) => Ok(&binder.name),
        ArrowParameter::Domain(_) => Err(Error::Unsupported(format!(
            "`#compile_ptx` target `{}` must name its PTX parameter",
            function.name.name
        ))),
    }
}

fn ptx_array_parameter_element_type(
    function: &neco_rs_parser::FunctionDeclaration,
) -> Result<ArrayElementType> {
    let Term::Arrow(arrow) = &function.ty else {
        unreachable!("validated PTX functions always have one parameter");
    };
    let ty = match &arrow.parameter {
        ArrowParameter::Binder(binder) => binder.ty.as_ref(),
        ArrowParameter::Domain(ty) => ty.as_ref(),
    };
    ptx_array_element_type(ty)?.ok_or_else(|| {
        Error::Unsupported(format!(
            "`#compile_ptx` target `{}` must receive an `ArrayVLPTX` parameter to use PTX array operations",
            function.name.name
        ))
    })
}

fn ptx_array_element_type(ty: &Term) -> Result<Option<ArrayElementType>> {
    let Term::Application { callee, arguments } = ty else {
        return Ok(None);
    };
    let Term::Path(path) = callee.as_ref() else {
        return Ok(None);
    };
    if path.token_keyword_package.is_some()
        || path.segments.len() != 1
        || path.segments[0].lexeme != "ArrayVLPTX"
    {
        return Ok(None);
    }
    let [element_type] = arguments.as_slice() else {
        return Err(Error::Unsupported(
            "`ArrayVLPTX` PTX parameter must receive exactly one element type".to_string(),
        ));
    };
    match simple_type_name(element_type) {
        Some("i32") => Ok(Some(ArrayElementType::I32)),
        Some("i64") => Ok(Some(ArrayElementType::I64)),
        Some("f32") => Ok(Some(ArrayElementType::F32)),
        Some("u8") => Ok(Some(ArrayElementType::U8)),
        _ => Err(Error::Unsupported(
            "`ArrayVLPTX` PTX parameter supports only i32/i64/f32/u8 elements".to_string(),
        )),
    }
}

fn ptx_ref_get_call_parts(term: &Term) -> Result<Option<(String, Term)>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    if !is_simple_callee(callee, "ref_get_ptx") {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, array, index] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_get_ptx` must receive a type, an `ArrayVLPTX`, and an index".to_string(),
        ));
    };
    let Some(array_name) = simple_path_name(array) else {
        return Err(Error::Unsupported(
            "`ref_get_ptx` currently requires a simple array parameter".to_string(),
        ));
    };
    Ok(Some((array_name.to_string(), index.clone())))
}

fn ptx_local_ref_get_call_parts(term: &Term) -> Result<Option<String>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    if !is_simple_callee(callee, "ref_get") {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, reference] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_get` must receive a type and a local reference".to_string(),
        ));
    };
    let Some(ref_name) = simple_path_name(reference) else {
        return Err(Error::Unsupported(
            "`ref_get` currently requires a simple local reference".to_string(),
        ));
    };
    Ok(Some(ref_name.to_string()))
}

fn ptx_ref_set_call_parts(term: &Term) -> Result<Option<(String, Term, Term)>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    if !is_simple_callee(callee, "ref_set_ptx") {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, array, index, value] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_set_ptx` must receive a type, an `ArrayVLPTX`, an index, and a value".to_string(),
        ));
    };
    let Some(array_name) = simple_path_name(array) else {
        return Err(Error::Unsupported(
            "`ref_set_ptx` currently requires a simple array parameter".to_string(),
        ));
    };
    Ok(Some((array_name.to_string(), index.clone(), value.clone())))
}

fn ptx_local_ref_set_call_parts(term: &Term) -> Result<Option<(String, Term)>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    if !is_simple_callee(callee, "ref_set") {
        return Ok(None);
    }
    let normalized = normalize_numeric_literal_arguments(arguments);
    let [_ty, reference, value] = normalized.as_slice() else {
        return Err(Error::Unsupported(
            "`ref_set` must receive a type, a local reference, and a value".to_string(),
        ));
    };
    let Some(ref_name) = simple_path_name(reference) else {
        return Err(Error::Unsupported(
            "`ref_set` currently requires a simple local reference".to_string(),
        ));
    };
    Ok(Some((ref_name.to_string(), value.clone())))
}

fn is_simple_callee(term: &Term, expected: &str) -> bool {
    simple_path_name(term) == Some(expected)
}

fn flatten_application(term: &Term) -> Option<(&Term, Vec<Term>)> {
    let Term::Application { callee, arguments } = term else {
        return None;
    };
    if let Some((root, mut flattened)) = flatten_application(callee) {
        flattened.extend(arguments.iter().cloned());
        Some((root, flattened))
    } else {
        Some((callee, arguments.clone()))
    }
}

fn ptx_scalar_type(ty: &Term) -> Option<ArrayElementType> {
    match simple_type_name(ty) {
        Some("i32") => Some(ArrayElementType::I32),
        Some("i64") => Some(ArrayElementType::I64),
        Some("f32") => Some(ArrayElementType::F32),
        Some("u8") => Some(ArrayElementType::U8),
        _ => None,
    }
}

fn ptx_element_size(ty: ArrayElementType) -> i32 {
    match ty {
        ArrayElementType::I32 | ArrayElementType::F32 => 4,
        ArrayElementType::I64 => 8,
        ArrayElementType::U8 => 1,
    }
}

fn ptx_memory_type(ty: ArrayElementType) -> &'static str {
    match ty {
        ArrayElementType::I32 => "u32",
        ArrayElementType::I64 => "u64",
        ArrayElementType::F32 => "f32",
        ArrayElementType::U8 => "u8",
    }
}

fn ptx_register_type(ty: ArrayElementType) -> &'static str {
    match ty {
        ArrayElementType::I32 | ArrayElementType::U8 => "u32",
        ArrayElementType::I64 => "u64",
        ArrayElementType::F32 => "f32",
    }
}

fn ptx_binary_primitive(name: &str) -> Option<(ArrayElementType, &'static str)> {
    match scalar_primitive(name) {
        Some(ScalarPrimitive::Binary { ty, op }) => {
            ptx_binary_instruction(ty, op).map(|instruction| (array_element_type(ty), instruction))
        }
        _ => None,
    }
}

fn ptx_unary_primitive(name: &str) -> Option<(ArrayElementType, &'static str)> {
    match scalar_primitive(name) {
        Some(ScalarPrimitive::Unary {
            ty: ScalarType::F32,
            op: ScalarUnaryOp::Sqrt,
        }) => Some((ArrayElementType::F32, "sqrt.rn.f32")),
        _ => None,
    }
}

fn ptx_comparison_primitive(name: &str) -> Option<(ArrayElementType, &'static str)> {
    match scalar_primitive(name) {
        Some(ScalarPrimitive::Comparison { ty, kind }) => {
            let ty = match ty {
                ScalarType::I32 => ArrayElementType::I32,
                ScalarType::F32 => ArrayElementType::F32,
                ScalarType::I64 | ScalarType::U8 => return None,
            };
            Some((ty, ptx_comparison_instruction(kind)))
        }
        _ => None,
    }
}

fn ptx_comparison_type(ty: ArrayElementType) -> &'static str {
    match ty {
        ArrayElementType::I32 => "s32",
        ArrayElementType::F32 => "f32",
        ArrayElementType::I64 | ArrayElementType::U8 => unreachable!(),
    }
}

fn ptx_conversion_primitive(
    name: &str,
) -> Option<(ArrayElementType, ArrayElementType, &'static str)> {
    match scalar_primitive(name) {
        Some(ScalarPrimitive::Conversion {
            source: ScalarType::I32,
            dest: ScalarType::F32,
        }) => Some((
            ArrayElementType::I32,
            ArrayElementType::F32,
            "cvt.rn.f32.s32",
        )),
        Some(ScalarPrimitive::Conversion {
            source: ScalarType::F32,
            dest: ScalarType::I32,
        }) => Some((
            ArrayElementType::F32,
            ArrayElementType::I32,
            "cvt.rzi.s32.f32",
        )),
        _ => None,
    }
}

fn array_element_type(ty: ScalarType) -> ArrayElementType {
    match ty {
        ScalarType::I32 => ArrayElementType::I32,
        ScalarType::I64 => ArrayElementType::I64,
        ScalarType::F32 => ArrayElementType::F32,
        ScalarType::U8 => ArrayElementType::U8,
    }
}

fn ptx_binary_instruction(ty: ScalarType, op: ScalarBinaryOp) -> Option<&'static str> {
    match (ty, op) {
        (ScalarType::I32, ScalarBinaryOp::Add) => Some("add.s32"),
        (ScalarType::I32, ScalarBinaryOp::Sub) => Some("sub.s32"),
        (ScalarType::I32, ScalarBinaryOp::Mul) => Some("mul.lo.s32"),
        (ScalarType::I32, ScalarBinaryOp::Div) => Some("div.s32"),
        (ScalarType::I32, ScalarBinaryOp::Mod) => Some("rem.s32"),
        (ScalarType::I32, ScalarBinaryOp::Xor) => Some("xor.b32"),
        (ScalarType::I32, ScalarBinaryOp::Shl) => Some("shl.b32"),
        (ScalarType::I32, ScalarBinaryOp::Shr) => Some("shr.u32"),
        (ScalarType::I64, ScalarBinaryOp::Add) => Some("add.s64"),
        (ScalarType::I64, ScalarBinaryOp::Sub) => Some("sub.s64"),
        (ScalarType::I64, ScalarBinaryOp::Mul) => Some("mul.lo.s64"),
        (ScalarType::I64, ScalarBinaryOp::Div) => Some("div.s64"),
        (ScalarType::F32, ScalarBinaryOp::Add) => Some("add.rn.f32"),
        (ScalarType::F32, ScalarBinaryOp::Sub) => Some("sub.rn.f32"),
        (ScalarType::F32, ScalarBinaryOp::Mul) => Some("mul.rn.f32"),
        (ScalarType::F32, ScalarBinaryOp::Div) => Some("div.rn.f32"),
        _ => None,
    }
}

fn ptx_comparison_instruction(kind: crate::ir::ComparisonKind) -> &'static str {
    match kind {
        crate::ir::ComparisonKind::Eq => "eq",
        crate::ir::ComparisonKind::Lte => "le",
        crate::ir::ComparisonKind::Lt => "lt",
        crate::ir::ComparisonKind::Gte => "ge",
        crate::ir::ComparisonKind::Gt => "gt",
    }
}

fn ptx_special_register(name: &str) -> Option<&'static str> {
    match name {
        "ctaid_x" => Some("%ctaid.x"),
        "ctaid_y" => Some("%ctaid.y"),
        "ctaid_z" => Some("%ctaid.z"),
        "ntid_x" => Some("%ntid.x"),
        "ntid_y" => Some("%ntid.y"),
        "ntid_z" => Some("%ntid.z"),
        "tid_x" => Some("%tid.x"),
        "tid_y" => Some("%tid.y"),
        "tid_z" => Some("%tid.z"),
        _ => None,
    }
}

fn simple_path_name(term: &Term) -> Option<&str> {
    let Term::Path(path) = term else {
        return None;
    };
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        Some(path.segments[0].lexeme.as_str())
    } else {
        None
    }
}

fn parse_ptx_i32_literal(term: &Term) -> Result<Option<i32>> {
    match term {
        Term::Application { callee, arguments } => {
            let [suffix] = arguments.as_slice() else {
                return Ok(None);
            };
            if simple_path_name(suffix) != Some("i32") {
                return Ok(None);
            }
            let Term::IntegerLiteral(literal) = callee.as_ref() else {
                return Ok(None);
            };
            literal
                .parse::<i32>()
                .map(Some)
                .map_err(|_| Error::Unsupported("PTX i32 literal is out of range".to_string()))
        }
        Term::IntegerLiteral(literal) => literal
            .parse::<i32>()
            .map(Some)
            .map_err(|_| Error::Unsupported("PTX i32 literal is out of range".to_string())),
        _ => Ok(None),
    }
}

fn parse_ptx_i64_literal(term: &Term) -> Result<Option<i64>> {
    match term {
        Term::Application { callee, arguments } => {
            let [suffix] = arguments.as_slice() else {
                return Ok(None);
            };
            if simple_path_name(suffix) != Some("i64") {
                return Ok(None);
            }
            let Term::IntegerLiteral(literal) = callee.as_ref() else {
                return Ok(None);
            };
            literal
                .parse::<i64>()
                .map(Some)
                .map_err(|_| Error::Unsupported("PTX i64 literal is out of range".to_string()))
        }
        _ => Ok(None),
    }
}

fn parse_ptx_f32_literal_bits(term: &Term) -> Result<Option<u32>> {
    let Term::Application { callee, arguments } = term else {
        return Ok(None);
    };
    let [suffix] = arguments.as_slice() else {
        return Ok(None);
    };
    if simple_path_name(suffix) != Some("f32") {
        return Ok(None);
    }
    let Term::IntegerLiteral(literal) = callee.as_ref() else {
        return Ok(None);
    };
    let value = literal
        .parse::<f32>()
        .map_err(|_| Error::Unsupported("PTX f32 literal could not be parsed".to_string()))?;
    Ok(Some(value.to_bits()))
}

fn simple_type_name(ty: &Term) -> Option<&str> {
    let Term::Path(path) = ty else {
        return None;
    };
    if path.token_keyword_package.is_none() && path.segments.len() == 1 {
        Some(path.segments[0].lexeme.as_str())
    } else {
        None
    }
}
