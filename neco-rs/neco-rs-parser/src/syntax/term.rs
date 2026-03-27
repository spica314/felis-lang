use crate::{Keyword, Parse, Parser, Result, TokenKind};

use super::{Item, PathExpression, Pattern};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Unit,
    StringLiteral(String),
    IntegerLiteral(String),
    Path(PathExpression),
    Group(Box<Term>),
    TypedBinder(TypedBinder),
    Block(Block),
    Match(MatchExpression),
    Application {
        callee: Box<Term>,
        arguments: Vec<Term>,
    },
    MethodCall {
        receiver: Box<Term>,
        method: String,
    },
    Arrow(ArrowTerm),
    Forall(ForallTerm),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypedBinder {
    pub name: String,
    pub ty: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrowTerm {
    pub parameter: ArrowParameter,
    pub result: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArrowParameter {
    Binder(TypedBinder),
    Domain(Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForallTerm {
    pub binder: TypedBinder,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub tail: Option<Box<Term>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let(LetStatement),
    Item(Box<Item>),
    Expression(Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: BindingPattern,
    pub operator: LetOperator,
    pub value: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingPattern {
    Name(String),
    Wildcard,
    ValueAndReference {
        value: Box<BindingPattern>,
        reference: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetOperator {
    Equals,
    LeftArrow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchExpression {
    pub scrutinee: Box<Term>,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub result: Box<Term>,
}

impl Parse for Term {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parse_arrow_term(parser)
    }
}

impl Parse for TypedBinder {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let name = parser.expect_identifier()?;
        parser.expect_punctuation(TokenKind::Colon)?;
        let ty = Term::parse(parser)?;
        Ok(Self {
            name,
            ty: Box::new(ty),
        })
    }
}

impl Parse for Block {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.expect_punctuation(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();
        let mut tail = None;

        while !parser.check_punctuation(TokenKind::RightBrace) {
            if parser.is_item_start() {
                statements.push(Statement::Item(Box::new(Item::parse(parser)?)));
                continue;
            }
            if parser.consume_keyword(Keyword::Let) {
                statements.push(Statement::Let(LetStatement::parse(parser)?));
                continue;
            }

            let expression = Term::parse(parser)?;
            if parser.consume_punctuation(TokenKind::Semicolon) {
                statements.push(Statement::Expression(Box::new(expression)));
            } else {
                tail = Some(Box::new(expression));
                break;
            }
        }

        parser.expect_punctuation(TokenKind::RightBrace)?;
        Ok(Self { statements, tail })
    }
}

impl Parse for LetStatement {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let binder = BindingPattern::parse(parser)?;
        let operator = if parser.consume_punctuation(TokenKind::Equals) {
            LetOperator::Equals
        } else {
            parser.expect_punctuation(TokenKind::LeftArrow)?;
            LetOperator::LeftArrow
        };
        let value = Term::parse(parser)?;
        parser.expect_punctuation(TokenKind::Semicolon)?;
        Ok(Self {
            binder,
            operator,
            value: Box::new(value),
        })
    }
}

impl Parse for BindingPattern {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let left = if parser.consume_punctuation(TokenKind::Underscore) {
            Self::Wildcard
        } else {
            Self::Name(parser.expect_identifier()?)
        };
        if parser.consume_punctuation(TokenKind::At) {
            let reference = parser.expect_identifier()?;
            Ok(Self::ValueAndReference {
                value: Box::new(left),
                reference,
            })
        } else {
            Ok(left)
        }
    }
}

impl Parse for MatchExpression {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let scrutinee = Term::parse(parser)?;
        parser.expect_punctuation(TokenKind::LeftBrace)?;
        let mut arms = Vec::new();
        while !parser.consume_punctuation(TokenKind::RightBrace) {
            arms.push(MatchArm::parse(parser)?);
        }
        Ok(Self {
            scrutinee: Box::new(scrutinee),
            arms,
        })
    }
}

impl Parse for MatchArm {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let pattern = Pattern::parse(parser)?;
        parser.expect_punctuation(TokenKind::FatArrow)?;
        let result = parser.with_match_arm_boundary(true, Term::parse)?;
        Ok(Self {
            pattern,
            result: Box::new(result),
        })
    }
}

fn parse_arrow_term(parser: &mut Parser) -> Result<Term> {
    let left = parse_forall_term(parser)?;
    if parser.consume_punctuation(TokenKind::Arrow) {
        let result = parse_arrow_term(parser)?;
        let parameter = match left {
            Term::TypedBinder(binder) => ArrowParameter::Binder(binder),
            other => ArrowParameter::Domain(Box::new(other)),
        };
        return Ok(Term::Arrow(ArrowTerm {
            parameter,
            result: Box::new(result),
        }));
    }
    Ok(left)
}

fn parse_forall_term(parser: &mut Parser) -> Result<Term> {
    if parser.consume_keyword(Keyword::Forall) {
        let binder = TypedBinder::parse(parser)?;
        parser.expect_punctuation(TokenKind::Comma)?;
        let body = Term::parse(parser)?;
        return Ok(Term::Forall(ForallTerm {
            binder,
            body: Box::new(body),
        }));
    }
    parse_application_term(parser)
}

fn parse_application_term(parser: &mut Parser) -> Result<Term> {
    let mut term = parse_primary_term(parser)?;
    loop {
        if parser.consume_punctuation(TokenKind::DotArrow) {
            let method = parser.expect_identifier()?;
            term = Term::MethodCall {
                receiver: Box::new(term),
                method,
            };
            continue;
        }
        if parser.stop_at_match_arm_boundary() && parser.looks_like_match_arm_boundary() {
            break;
        }
        if parser.is_term_start() {
            let argument = parse_primary_term(parser)?;
            term = match term {
                Term::Application {
                    callee,
                    mut arguments,
                } => {
                    arguments.push(argument);
                    Term::Application { callee, arguments }
                }
                other => Term::Application {
                    callee: Box::new(other),
                    arguments: vec![argument],
                },
            };
            continue;
        }
        break;
    }
    Ok(term)
}

fn parse_primary_term(parser: &mut Parser) -> Result<Term> {
    if parser.consume_keyword(Keyword::Match) {
        return Ok(Term::Match(MatchExpression::parse(parser)?));
    }

    if parser.check_punctuation(TokenKind::LeftBrace) {
        return Ok(Term::Block(Block::parse(parser)?));
    }

    if parser.consume_punctuation(TokenKind::LeftParen) {
        if parser.consume_punctuation(TokenKind::RightParen) {
            return Ok(Term::Unit);
        }

        if parser.looks_like_typed_binder() {
            let binder = TypedBinder::parse(parser)?;
            parser.expect_punctuation(TokenKind::RightParen)?;
            return Ok(Term::TypedBinder(binder));
        }

        let inner = Term::parse(parser)?;
        parser.expect_punctuation(TokenKind::RightParen)?;
        return Ok(Term::Group(Box::new(inner)));
    }

    if let Some(text) = parser.consume_string_literal() {
        return Ok(Term::StringLiteral(text));
    }

    if let Some(number) = parser.consume_integer_literal() {
        return Ok(Term::IntegerLiteral(number));
    }

    if parser.is_path_start() {
        return Ok(Term::Path(PathExpression::parse(parser)?));
    }

    Err(parser.error_here("expected term"))
}
