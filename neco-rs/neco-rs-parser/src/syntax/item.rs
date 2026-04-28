use crate::lexer::{
    TokenColon, TokenComma, TokenIdentifier, TokenKeyword, TokenKeywordKind, TokenLeftBrace,
    TokenLeftBracket, TokenLeftParen, TokenRightBrace, TokenRightBracket, TokenRightParen,
    TokenSemicolon, TokenStringLiteral,
};
use crate::{Error, Parse, Result, Token};

use super::{Block, PathExpression, Term, TermParseOption};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    EntryPoint(EntryPointDeclaration),
    Use(UseDeclaration),
    Mod(ModuleDeclaration),
    BindBuiltin(BindBuiltinDeclaration),
    Function(FunctionDeclaration),
    Struct(StructDeclaration),
    Type(TypeDeclaration),
    Prop(PropDeclaration),
    Theorem(TheoremDeclaration),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EntryPointDeclaration {
    pub token_keyword_entrypoint: TokenKeyword,
    pub token_ident: TokenIdentifier,
    pub token_semi: TokenSemicolon,
}

impl Parse for EntryPointDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(token_keyword_entrypoint) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::EntryPoint))?
        else {
            return Ok(None);
        };

        let Some(token_ident) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("entrypoint name"));
        };

        let Some(token_semi) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after entrypoint declaration"));
        };

        *i = k;
        Ok(Some(EntryPointDeclaration {
            token_keyword_entrypoint,
            token_ident,
            token_semi,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub path: PathExpression,
}

impl Parse for UseDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;

        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Use))?
        else {
            return Ok(None);
        };
        let Some(path) = PathExpression::parse(tokens, &mut k)? else {
            return Err(expected("path after `use`"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after use declaration"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            path,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: String,
}

impl Parse for ModuleDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;

        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Mod))?
        else {
            return Ok(None);
        };
        let Some(name) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("module name"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after module declaration"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name: name.lexeme,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindBuiltinDeclaration {
    pub builtin_name: String,
    pub alias: String,
}

impl Parse for BindBuiltinDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::BindBuiltin))?
        else {
            return Ok(None);
        };
        let Some(builtin_name) = TokenStringLiteral::parse(tokens, &mut k)? else {
            return Err(expected("builtin name string literal"));
        };
        let Some(_) = TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::As))?
        else {
            return Err(expected("`as` after builtin name"));
        };
        let Some(alias) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Err(expected("bind-builtin alias"));
        };
        let Some(_) = TokenSemicolon::parse(tokens, &mut k)? else {
            return Err(expected("`;` after bind-builtin declaration"));
        };

        *i = k;
        Ok(Some(Self {
            builtin_name: builtin_name.lexeme,
            alias: alias.lexeme,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub kind: FunctionKind,
    pub name: DeclaredName,
    pub ty: Term,
    pub effect: Option<TokenIdentifier>,
    pub body: Block,
}

impl Parse for FunctionDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let kind = if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Fn))?
            .is_some()
        {
            FunctionKind::Fn
        } else if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Proc))?
            .is_some()
        {
            FunctionKind::Proc
        } else {
            return Ok(None);
        };
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("function name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after function name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("function type"));
        };
        let effect =
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::With))?
                .is_some()
            {
                Some(
                    TokenIdentifier::parse(tokens, &mut k)?
                        .ok_or_else(|| expected("function effect after `with`"))?,
                )
            } else {
                None
            };
        let Some(body) = Block::parse(tokens, &mut k)? else {
            return Err(expected("function body"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            kind,
            name,
            ty,
            effect,
            body,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub modifier: Option<String>,
    pub ty: Term,
    pub fields: Vec<StructFieldDeclaration>,
}

impl Parse for StructDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Struct))?
        else {
            return Ok(None);
        };
        let modifier = parse_modifier(tokens, &mut k)?;
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("struct name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after struct name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("struct type"));
        };
        let Some(fields) = parse_struct_field_block(tokens, &mut k)? else {
            return Err(expected("struct field block"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            modifier,
            ty,
            fields,
        }))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionKind {
    Fn,
    Proc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub modifier: Option<String>,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

impl Parse for TypeDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Type))?
        else {
            return Ok(None);
        };
        let modifier = parse_modifier(tokens, &mut k)?;
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("type name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after type name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("type kind"));
        };
        let Some(constructors) = parse_constructor_block(tokens, &mut k)? else {
            return Err(expected("constructor block"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            modifier,
            ty,
            constructors,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PropDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub ty: Term,
    pub constructors: Vec<ConstructorDeclaration>,
}

impl Parse for PropDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Prop))?
        else {
            return Ok(None);
        };
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("prop name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after prop name"));
        };
        let Some(ty) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("prop type"));
        };
        let Some(constructors) = parse_constructor_block(tokens, &mut k)? else {
            return Err(expected("prop constructor block"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            ty,
            constructors,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TheoremDeclaration {
    pub token_keyword_pub: Option<TokenKeyword>,
    pub name: DeclaredName,
    pub statement: Term,
    pub body: Block,
}

impl Parse for TheoremDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let token_keyword_pub =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Pub))?;
        let Some(_) =
            TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Theorem))?
        else {
            return Ok(None);
        };
        let Some(name) = DeclaredName::parse(tokens, &mut k)? else {
            return Err(expected("theorem name"));
        };
        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after theorem name"));
        };
        let Some(statement) = parse_term_before_left_brace(tokens, &mut k)? else {
            return Err(expected("theorem statement"));
        };
        let Some(body) = Block::parse(tokens, &mut k)? else {
            return Err(expected("theorem body"));
        };

        *i = k;
        Ok(Some(Self {
            token_keyword_pub,
            name,
            statement,
            body,
        }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstructorDeclaration {
    pub name: DeclaredName,
    pub ty: Term,
}

impl Parse for ConstructorDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let Some(name) = DeclaredName::parse(tokens, i)? else {
            return Ok(None);
        };
        let Some(_) = TokenColon::parse(tokens, i)? else {
            return Err(expected("`:` after constructor name"));
        };
        let Some(ty) = Term::parse(tokens, i)? else {
            return Err(expected("constructor type"));
        };
        Ok(Some(Self { name, ty }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructFieldDeclaration {
    pub token_ident: TokenIdentifier,
    pub ty: Term,
}

impl Parse for StructFieldDeclaration {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;

        let Some(token_ident) = TokenIdentifier::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let Some(_) = TokenColon::parse(tokens, &mut k)? else {
            return Err(expected("`:` after struct field name"));
        };

        let Some(ty) = Term::parse(tokens, &mut k)? else {
            return Err(expected("struct field type"));
        };

        *i = k;
        Ok(Some(Self { token_ident, ty }))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DeclaredName {
    pub name: String,
    pub suffixes: Vec<Term>,
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

impl Parse for DeclaredName {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let Some(name) = TokenIdentifier::parse(tokens, i)? else {
            return Ok(None);
        };
        let suffixes = parse_suffixes(tokens, i)?;
        Ok(Some(Self {
            name: name.lexeme,
            suffixes,
        }))
    }
}

fn parse_constructor_block(
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

fn parse_struct_field_block(
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

fn parse_modifier(tokens: &[Token], i: &mut usize) -> Result<Option<String>> {
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

fn parse_suffixes(tokens: &[Token], i: &mut usize) -> Result<Vec<Term>> {
    let mut suffixes = Vec::new();
    while TokenLeftBracket::parse(tokens, i)?.is_some() {
        let Some(suffix) = Term::parse(tokens, i)? else {
            return Err(expected("declared-name suffix term"));
        };
        let Some(_) = TokenRightBracket::parse(tokens, i)? else {
            return Err(expected("`]` after declared-name suffix"));
        };
        suffixes.push(suffix);
    }
    Ok(suffixes)
}

fn parse_term_before_left_brace(tokens: &[Token], i: &mut usize) -> Result<Option<Term>> {
    Term::parse_with_option(
        tokens,
        i,
        Some(TermParseOption {
            stop_at_left_brace: true,
            stop_at_match_arm_boundary: false,
        }),
    )
}

fn expected(what: &str) -> Error {
    Error::Message(format!("expected {what}"))
}
