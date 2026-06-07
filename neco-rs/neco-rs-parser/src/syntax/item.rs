mod bind_builtin_declaration;
mod compile_ptx_declaration;
mod constructor_declaration;
mod declared_name;
mod entry_point_declaration;
mod function_declaration;
mod item;
mod module_declaration;
mod prop_declaration;
mod struct_declaration;
mod struct_field_declaration;
mod theorem_declaration;
mod type_declaration;
mod use_declaration;

pub use bind_builtin_declaration::BindBuiltinDeclaration;
pub use compile_ptx_declaration::CompilePtxDeclaration;
pub use constructor_declaration::ConstructorDeclaration;
pub use declared_name::DeclaredName;
pub use entry_point_declaration::EntryPointDeclaration;
pub use function_declaration::FunctionDeclaration;
pub use item::Item;
pub use module_declaration::ModuleDeclaration;
pub use prop_declaration::PropDeclaration;
pub use struct_declaration::StructDeclaration;
pub use struct_field_declaration::StructFieldDeclaration;
pub use theorem_declaration::TheoremDeclaration;
pub use type_declaration::TypeDeclaration;
pub use use_declaration::UseDeclaration;

use crate::lexer::{
    TokenComma, TokenIdentifier, TokenLeftBrace, TokenLeftParen, TokenRightBrace, TokenRightParen,
};
use crate::{Error, Parse, Result, Token};

use super::{Term, TermParseOption};

pub(crate) fn parse_constructor_block(
    tokens: &[Token],
    i: &mut usize,
) -> Result<Option<Vec<ConstructorDeclaration>>> {
    let Some(_) = TokenLeftBrace::parse(tokens, i)? else {
        return Ok(None);
    };
    let mut constructors = Vec::new();
    while TokenRightBrace::parse(tokens, i)?.is_none() {
        let Some(constructor) = ConstructorDeclaration::parse(tokens, i)? else {
            return Err(expected("constructor declaration"));
        };
        let Some(_) = TokenComma::parse(tokens, i)? else {
            return Err(expected("`,` after constructor declaration"));
        };
        constructors.push(constructor);
    }
    Ok(Some(constructors))
}

pub(crate) fn parse_struct_field_block(
    tokens: &[Token],
    i: &mut usize,
) -> Result<Option<Vec<StructFieldDeclaration>>> {
    let Some(_) = TokenLeftBrace::parse(tokens, i)? else {
        return Ok(None);
    };
    let mut fields = Vec::new();
    while TokenRightBrace::parse(tokens, i)?.is_none() {
        let Some(field) = StructFieldDeclaration::parse(tokens, i)? else {
            return Err(expected("struct field declaration"));
        };
        let Some(_) = TokenComma::parse(tokens, i)? else {
            return Err(expected("`,` after struct field declaration"));
        };
        fields.push(field);
    }
    Ok(Some(fields))
}

pub(crate) fn parse_modifier(tokens: &[Token], i: &mut usize) -> Result<Option<String>> {
    let mut k = *i;
    if TokenLeftParen::parse(tokens, &mut k)?.is_none() {
        return Ok(None);
    }
    let Some(modifier) = TokenIdentifier::parse(tokens, &mut k)? else {
        return Err(expected("modifier name"));
    };
    let Some(_) = TokenRightParen::parse(tokens, &mut k)? else {
        return Err(expected("`)` after modifier"));
    };
    *i = k;
    Ok(Some(modifier.lexeme))
}

pub(crate) fn parse_term_before_left_brace(
    tokens: &[Token],
    i: &mut usize,
) -> Result<Option<Term>> {
    Term::parse_with_option(
        tokens,
        i,
        Some(TermParseOption {
            stop_at_left_brace: true,
            stop_at_match_arm_boundary: false,
        }),
    )
}

pub(crate) fn expected(what: &str) -> Error {
    Error::Message(format!("expected {what}"))
}
