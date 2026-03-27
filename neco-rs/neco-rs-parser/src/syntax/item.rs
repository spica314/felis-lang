use crate::{Keyword, Parse, Parser, Result, TokenKind};

use super::attribute::{parse_attributes, parse_visibility};
use super::path::parse_suffixes;
use super::{Attribute, Block, PathExpression, Term};

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
    pub attributes: Vec<Attribute>,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub path: PathExpression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDeclaration {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindBuiltinDeclaration {
    pub attributes: Vec<Attribute>,
    pub builtin_name: String,
    pub alias: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub ty: Term,
    pub effect: Option<Term>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub modifier: Option<String>,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropDeclaration {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: DeclaredName,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TheoremDeclaration {
    pub attributes: Vec<Attribute>,
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
    fn parse(parser: &mut Parser) -> Result<Self> {
        let attributes = parse_attributes(parser)?;
        let visibility = parse_visibility(parser);

        if parser.consume_keyword(Keyword::EntryPoint) {
            let name = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Self::EntryPoint(EntryPointDeclaration { attributes, name }));
        }
        if parser.consume_keyword(Keyword::Use) {
            let path = PathExpression::parse(parser)?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Self::Use(UseDeclaration {
                attributes,
                visibility,
                path,
            }));
        }
        if parser.consume_keyword(Keyword::Mod) {
            let name = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Self::Mod(ModuleDeclaration {
                attributes,
                visibility,
                name,
            }));
        }
        if parser.consume_keyword(Keyword::BindBuiltin) {
            let builtin_name = parser.expect_string_literal()?;
            parser.expect_keyword(Keyword::As)?;
            let alias = parser.expect_identifier()?;
            parser.expect_punctuation(TokenKind::Semicolon)?;
            return Ok(Self::BindBuiltin(BindBuiltinDeclaration {
                attributes,
                builtin_name,
                alias,
            }));
        }
        if parser.consume_keyword(Keyword::Fn) {
            let name = DeclaredName::parse(parser)?;
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?;
            let effect = if parser.consume_keyword(Keyword::With) {
                Some(Term::parse(parser)?)
            } else {
                None
            };
            let body = Block::parse(parser)?;
            return Ok(Self::Function(FunctionDeclaration {
                attributes,
                visibility,
                name,
                ty,
                effect,
                body,
            }));
        }
        if parser.consume_keyword(Keyword::Type) {
            let modifier = if parser.consume_punctuation(TokenKind::LeftParen) {
                let modifier = parser.expect_identifier()?;
                parser.expect_punctuation(TokenKind::RightParen)?;
                Some(modifier)
            } else {
                None
            };
            let name = DeclaredName::parse(parser)?;
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?;
            let constructors = parse_constructor_block(parser)?;
            return Ok(Self::Type(TypeDeclaration {
                attributes,
                visibility,
                name,
                modifier,
                ty,
                constructors,
            }));
        }
        if parser.consume_keyword(Keyword::Prop) {
            let name = DeclaredName::parse(parser)?;
            parser.expect_punctuation(TokenKind::Colon)?;
            let ty = Term::parse(parser)?;
            let constructors = parse_constructor_block(parser)?;
            return Ok(Self::Prop(PropDeclaration {
                attributes,
                visibility,
                name,
                ty,
                constructors,
            }));
        }
        if parser.consume_keyword(Keyword::Theorem) {
            let name = DeclaredName::parse(parser)?;
            parser.expect_punctuation(TokenKind::Colon)?;
            let statement = Term::parse(parser)?;
            let body = Block::parse(parser)?;
            return Ok(Self::Theorem(TheoremDeclaration {
                attributes,
                visibility,
                name,
                statement,
                body,
            }));
        }

        Err(parser.error_here("expected declaration"))
    }
}

impl Parse for DeclaredName {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let name = parser.expect_identifier()?;
        let suffixes = parse_suffixes(parser)?;
        Ok(Self { name, suffixes })
    }
}

impl Parse for ConstructorDeclaration {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let name = DeclaredName::parse(parser)?;
        parser.expect_punctuation(TokenKind::Colon)?;
        let ty = Term::parse(parser)?;
        Ok(Self { name, ty })
    }
}

fn parse_constructor_block(parser: &mut Parser) -> Result<Vec<ConstructorDeclaration>> {
    parser.expect_punctuation(TokenKind::LeftBrace)?;
    let mut constructors = Vec::new();
    while !parser.consume_punctuation(TokenKind::RightBrace) {
        let constructor = ConstructorDeclaration::parse(parser)?;
        parser.expect_punctuation(TokenKind::Comma)?;
        constructors.push(constructor);
    }
    Ok(constructors)
}
