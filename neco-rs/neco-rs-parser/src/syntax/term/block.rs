use super::{
    IfStatement, LetRefStatement, LetStatement, LoopStatement,
    parse::{expect_right_brace, expect_semicolon, expected},
};
use super::{Statement, Term};
use crate::lexer::{
    TokenKeyword, TokenKeywordKind, TokenLeftBrace, TokenRightBrace, TokenSemicolon,
};
use crate::syntax::Item;
use crate::{Parse, Result, Token};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Option<Box<Term>>,
}

impl Parse for Block {
    type ParseOption = ();

    fn parse_with_option(
        tokens: &[Token],
        i: &mut usize,
        _: Option<Self::ParseOption>,
    ) -> Result<Option<Self>> {
        let mut k = *i;
        let Some(_) = TokenLeftBrace::parse(tokens, &mut k)? else {
            return Ok(None);
        };

        let mut statements = Vec::new();
        let mut tail = None;
        while TokenRightBrace::parse(tokens, &mut k)?.is_none() {
            let mut item_k = k;
            if let Some(item) = Item::parse(tokens, &mut item_k)? {
                k = item_k;
                statements.push(Statement::Item(Box::new(item)));
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Let))?
                .is_some()
            {
                let Some(statement) = LetStatement::parse(tokens, &mut k)? else {
                    return Err(expected("let statement"));
                };
                statements.push(Statement::Let(statement));
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::LetRef))?
                .is_some()
            {
                let Some(statement) = LetRefStatement::parse(tokens, &mut k)? else {
                    return Err(expected("let-ref statement"));
                };
                statements.push(Statement::LetRef(statement));
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::If))?
                .is_some()
            {
                let Some(statement) = IfStatement::parse(tokens, &mut k)? else {
                    return Err(expected("if statement"));
                };
                statements.push(Statement::If(statement));
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Loop))?
                .is_some()
            {
                let Some(statement) = LoopStatement::parse(tokens, &mut k)? else {
                    return Err(expected("loop statement"));
                };
                statements.push(Statement::Loop(statement));
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Break))?
                .is_some()
            {
                expect_semicolon(tokens, &mut k)?;
                statements.push(Statement::Break);
                continue;
            }
            if TokenKeyword::parse_with_option(tokens, &mut k, Some(TokenKeywordKind::Continue))?
                .is_some()
            {
                expect_semicolon(tokens, &mut k)?;
                statements.push(Statement::Continue);
                continue;
            }

            let Some(expression) = Term::parse(tokens, &mut k)? else {
                return Err(expected("block statement or tail expression"));
            };
            if TokenSemicolon::parse(tokens, &mut k)?.is_some() {
                statements.push(Statement::Expression(Box::new(expression)));
            } else {
                tail = Some(Box::new(expression));
                expect_right_brace(tokens, &mut k)?;
                break;
            }
        }

        *i = k;
        Ok(Some(Self { statements, tail }))
    }
}
