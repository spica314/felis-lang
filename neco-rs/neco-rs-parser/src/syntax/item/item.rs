use crate::{Parse, Result, Token};

use super::{
    BindBuiltinDeclaration, CompilePtxDeclaration, EntryPointDeclaration, FunctionDeclaration,
    ModuleDeclaration, PropDeclaration, StructDeclaration, TheoremDeclaration, TypeDeclaration,
    UseDeclaration,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    EntryPoint(EntryPointDeclaration),
    Use(UseDeclaration),
    Mod(ModuleDeclaration),
    BindBuiltin(BindBuiltinDeclaration),
    CompilePtx(CompilePtxDeclaration),
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
    Type(TypeDeclaration),
    Prop(PropDeclaration),
    Theorem(TheoremDeclaration),
}

impl Parse for Item {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        if let Some(entrypoint) = EntryPointDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::EntryPoint(entrypoint)));
        }
        if let Some(use_declaration) = UseDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Use(use_declaration)));
        }
        if let Some(module) = ModuleDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Mod(module)));
        }
        if let Some(bind_builtin) = BindBuiltinDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::BindBuiltin(bind_builtin)));
        }
        if let Some(compile_ptx) = CompilePtxDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::CompilePtx(compile_ptx)));
        }
        if let Some(function) = FunctionDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Function(function)));
        }
        if let Some(struct_declaration) = StructDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Struct(struct_declaration)));
        }
        if let Some(type_declaration) = TypeDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Type(type_declaration)));
        }
        if let Some(prop) = PropDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Prop(prop)));
        }
        if let Some(theorem) = TheoremDeclaration::parse(tokens, &mut k)? {
            *i = k;
            return Ok(Some(Self::Theorem(theorem)));
        }

        Ok(None)
    }
}
