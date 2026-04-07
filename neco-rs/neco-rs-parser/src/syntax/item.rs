use crate::{Keyword, Parse, Parser, Result, TokenKind};

use super::attribute::parse_visibility;
use super::path::parse_suffixes;
use super::{Block, PathExpression, Term};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    EntryPoint(EntryPointDeclaration),
    Use(UseDeclaration),
    Mod(ModuleDeclaration),
    BindBuiltin(BindBuiltinDeclaration),
    Function(FunctionDeclaration),
    Type(TypeDeclaration),
    Prop(PropDeclaration),
    Theorem(TheoremDeclaration),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryPointDeclaration {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub visibility: Visibility,
    pub path: PathExpression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDeclaration {
    pub visibility: Visibility,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindBuiltinDeclaration {
    pub builtin_name: String,
    pub alias: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub visibility: Visibility,
    pub kind: FunctionKind,
    pub name: DeclaredName,
    pub ty: Term,
    pub effect: Option<Term>,
    pub body: Block,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Fn,
    Proc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub modifier: Option<String>,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropDeclaration {
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TheoremDeclaration {
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub statement: Term,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorDeclaration {
    pub name: DeclaredName,
    pub ty: Term,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredName {
    pub name: String,
    pub suffixes: Vec<Term>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Private,
    Public,
}

impl Parse for Item {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let visibility = parse_visibility(parser);

        if parser.consume_keyword(Keyword::EntryPoint) {
            let name = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Some(Self::EntryPoint(EntryPointDeclaration { name })));
        }
        if parser.consume_keyword(Keyword::Use) {
            let path = PathExpression::parse(parser)?.unwrap();
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Some(Self::Use(UseDeclaration { visibility, path })));
        }
        if parser.consume_keyword(Keyword::Mod) {
            let name = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Some(Self::Mod(ModuleDeclaration { visibility, name })));
        }
        if parser.consume_keyword(Keyword::BindBuiltin) {
            let builtin_name = parser.expect_string_literal()?;
            parser.expect_keyword(Keyword::As)?;
            let alias = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Some(Self::BindBuiltin(BindBuiltinDeclaration {
                builtin_name,
                alias,
            })));
        }
        let kind = if parser.consume_keyword(Keyword::Fn) {
            Some(FunctionKind::Fn)
        } else if parser.consume_keyword(Keyword::Proc) {
            Some(FunctionKind::Proc)
        } else {
            None
        };
        if let Some(kind) = kind {
            let name = DeclaredName::parse(parser)?.unwrap();
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?.unwrap();
            let effect = if parser.consume_keyword(Keyword::With) {
                Some(Term::parse(parser)?.unwrap())
            } else {
                None
            };
            let body = Block::parse(parser)?.unwrap();
            return Ok(Some(Self::Function(FunctionDeclaration {
                visibility,
                kind,
                name,
                ty,
                effect,
                body,
            })));
        }
        if parser.consume_keyword(Keyword::Type) {
            let modifier = if parser.consume_punctuation(TokenKind::LeftParen) {
                let modifier = parser.expect_identifier()?;
                parser.expect_punctuation(TokenKind::RightParen)?;
                Some(modifier)
            } else {
                None
            };
            let name = DeclaredName::parse(parser)?.unwrap();
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?.unwrap();
            let constructors = parse_constructor_block(parser)?.unwrap();
            return Ok(Some(Self::Type(TypeDeclaration {
                visibility,
                name,
                modifier,
                ty,
                constructors,
            })));
        }
        if parser.consume_keyword(Keyword::Prop) {
            let name = DeclaredName::parse(parser)?.unwrap();
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?.unwrap();
            let constructors = parse_constructor_block(parser)?.unwrap();
            return Ok(Some(Self::Prop(PropDeclaration {
                visibility,
                name,
                ty,
                constructors,
            })));
        }
        if parser.consume_keyword(Keyword::Theorem) {
            let name = DeclaredName::parse(parser)?.unwrap();
            parser.expect_punctuation(TokenKind::Colon)?;
            let statement = Term::parse(parser)?.unwrap();
            let body = Block::parse(parser)?.unwrap();
            return Ok(Some(Self::Theorem(TheoremDeclaration {
                visibility,
                name,
                statement,
                body,
            })));
        }

        Err(parser.error_here("expected declaration"))
    }
}

impl Parse for DeclaredName {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let name = parser.expect_identifier()?;
        let suffixes = parse_suffixes(parser)?;
        Ok(Some(Self { name, suffixes }))
    }
}

impl Parse for ConstructorDeclaration {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let name = DeclaredName::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::Colon)?;
        let ty = Term::parse(parser)?.unwrap();
        Ok(Some(Self { name, ty }))
    }
}

fn parse_constructor_block(parser: &mut Parser) -> Result<Option<Vec<ConstructorDeclaration>>> {
    parser.expect_punctuation(TokenKind::LeftBrace)?;
    let mut constructors = Vec::new();
    while !parser.consume_punctuation(TokenKind::RightBrace) {
        let constructor = ConstructorDeclaration::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::Comma)?;
        constructors.push(constructor);
    }
    Ok(Some(constructors))
}
