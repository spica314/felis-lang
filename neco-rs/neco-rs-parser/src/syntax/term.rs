use crate::{Keyword, Parse, Parser, Result, TokenKind};

use super::{Item, PathExpression, Pattern};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Unit,
    StringLiteral(String),
    CharLiteral(char),
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
    If(IfStatement),
    Loop(LoopStatement),
    Break,
    Continue,
    Item(Box<Item>),
    Expression(Box<Term>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfStatement {
    pub condition: Box<Term>,
    pub then_block: Block,
    pub else_block: Option<Block>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
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
        exclusive: bool,
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
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        parse_arrow_term(parser)
    }
}

impl Parse for TypedBinder {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let name = parser.expect_identifier()?;
        parser.expect_punctuation(TokenKind::Colon)?;
        let ty = Term::parse(parser)?.unwrap();
        Ok(Some(Self {
            name,
            ty: Box::new(ty),
        }))
    }
}

impl Parse for Block {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        parser.expect_punctuation(TokenKind::LeftBrace)?;
        let mut statements = Vec::new();
        let mut tail = None;

        while !parser.check_punctuation(TokenKind::RightBrace) {
            if parser.is_item_start() {
                statements.push(Statement::Item(Box::new(Item::parse(parser)?.unwrap())));
                continue;
            }
            if parser.consume_keyword(Keyword::Let) {
                statements.push(Statement::Let(LetStatement::parse(parser)?.unwrap()));
                continue;
            }
            if parser.consume_keyword(Keyword::If) {
                statements.push(Statement::If(IfStatement::parse(parser)?.unwrap()));
                continue;
            }
            if parser.consume_keyword(Keyword::Loop) {
                statements.push(Statement::Loop(LoopStatement::parse(parser)?.unwrap()));
                continue;
            }
            if parser.consume_keyword(Keyword::Break) {
                parser.expect_punctuation(TokenKind::Semicolon)?;
                statements.push(Statement::Break);
                continue;
            }
            if parser.consume_keyword(Keyword::Continue) {
                parser.expect_punctuation(TokenKind::Semicolon)?;
                statements.push(Statement::Continue);
                continue;
            }

            let expression = Term::parse(parser)?.unwrap();
            if parser.consume_punctuation(TokenKind::Semicolon) {
                statements.push(Statement::Expression(Box::new(expression)));
            } else {
                tail = Some(Box::new(expression));
                break;
            }
        }

        parser.expect_punctuation(TokenKind::RightBrace)?;
        Ok(Some(Self { statements, tail }))
    }
}

impl Parse for LetStatement {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let binder = BindingPattern::parse(parser)?.unwrap();
        let operator = if parser.consume_punctuation(TokenKind::Equals) {
            LetOperator::Equals
        } else {
            parser.expect_punctuation(TokenKind::LeftArrow)?;
            LetOperator::LeftArrow
        };
        let value = Term::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::Semicolon)?;
        Ok(Some(Self {
            binder,
            operator,
            value: Box::new(value),
        }))
    }
}

impl Parse for IfStatement {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let condition = parser.with_left_brace_boundary(true, Term::parse)?.unwrap();
        let then_block = Block::parse(parser)?.unwrap();
        let else_block = if parser.consume_keyword(Keyword::Else) {
            Some(Block::parse(parser)?.unwrap())
        } else {
            None
        };
        parser.expect_punctuation(TokenKind::Semicolon)?;
        Ok(Some(Self {
            condition: Box::new(condition),
            then_block,
            else_block,
        }))
    }
}

impl Parse for LoopStatement {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let body = Block::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::Semicolon)?;
        Ok(Some(Self { body }))
    }
}

impl Parse for BindingPattern {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let left = if parser.consume_punctuation(TokenKind::Underscore) {
            Self::Wildcard
        } else {
            Self::Name(parser.expect_identifier()?)
        };
        let exclusive = if parser.consume_punctuation(TokenKind::At) {
            false
        } else if parser.consume_punctuation(TokenKind::AtCaret) {
            true
        } else {
            return Ok(Some(left));
        };
        {
            let reference = parser.expect_identifier()?;
            Ok(Some(Self::ValueAndReference {
                value: Box::new(left),
                reference,
                exclusive,
            }))
        }
    }
}

impl Parse for MatchExpression {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let scrutinee = Term::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::LeftBrace)?;
        let mut arms = Vec::new();
        while !parser.consume_punctuation(TokenKind::RightBrace) {
            arms.push(MatchArm::parse(parser)?.unwrap());
        }
        Ok(Some(Self {
            scrutinee: Box::new(scrutinee),
            arms,
        }))
    }
}

impl Parse for MatchArm {
    fn parse(parser: &mut Parser) -> Result<Option<Self>> {
        let pattern = Pattern::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::FatArrow)?;
        let result = parser.with_match_arm_boundary(true, Term::parse)?.unwrap();
        if !matches!(result, Term::Block(_)) {
            parser.expect_punctuation(TokenKind::Comma)?;
        }
        Ok(Some(Self {
            pattern,
            result: Box::new(result),
        }))
    }
}

fn parse_arrow_term(parser: &mut Parser) -> Result<Option<Term>> {
    let left = parse_forall_term(parser)?;
    if parser.consume_punctuation(TokenKind::Arrow) {
        let result = parse_arrow_term(parser)?.unwrap();
        let parameter = match left {
            Term::TypedBinder(binder) => ArrowParameter::Binder(binder),
            other => ArrowParameter::Domain(Box::new(other)),
        };
        return Ok(Some(Term::Arrow(ArrowTerm {
            parameter,
            result: Box::new(result),
        })));
    }
    Ok(Some(left))
}

fn parse_forall_term(parser: &mut Parser) -> Result<Term> {
    if parser.consume_keyword(Keyword::Forall) {
        let binder = TypedBinder::parse(parser)?.unwrap();
        parser.expect_punctuation(TokenKind::Comma)?;
        let body = Term::parse(parser)?.unwrap();
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
        if parser.stop_at_left_brace() && parser.check_punctuation(TokenKind::LeftBrace) {
            break;
        }
        if parser.stop_at_match_arm_boundary()
            && parser.looks_like_match_arm_boundary()
            && !looks_like_numeric_literal_suffix_continuation(&term, parser)
        {
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

fn looks_like_numeric_literal_suffix_continuation(term: &Term, parser: &Parser) -> bool {
    matches!(term, Term::IntegerLiteral(_))
        && matches!(
            parser.peek_kind(),
            TokenKind::Identifier(name) if name == "i32" || name == "u8"
        )
}

fn parse_primary_term(parser: &mut Parser) -> Result<Term> {
    if parser.consume_keyword(Keyword::Match) {
        return Ok(Term::Match(MatchExpression::parse(parser)?.unwrap()));
    }

    if parser.check_punctuation(TokenKind::LeftBrace) {
        return Ok(Term::Block(Block::parse(parser)?.unwrap()));
    }

    if parser.consume_punctuation(TokenKind::LeftParen) {
        if parser.consume_punctuation(TokenKind::RightParen) {
            return Ok(Term::Unit);
        }

        if parser.looks_like_typed_binder() {
            let binder = TypedBinder::parse(parser)?;
            parser.expect_punctuation(TokenKind::RightParen)?;
            return Ok(Term::TypedBinder(binder.unwrap()));
        }

        let inner = Term::parse(parser)?;
        parser.expect_punctuation(TokenKind::RightParen)?;
        return Ok(Term::Group(Box::new(inner.unwrap())));
    }

    if let Some(text) = parser.consume_string_literal() {
        return Ok(Term::StringLiteral(text));
    }

    if let Some(ch) = parser.consume_char_literal() {
        return Ok(Term::CharLiteral(ch));
    }

    if let Some(number) = parser.consume_integer_literal() {
        return Ok(Term::IntegerLiteral(number));
    }

    if parser.is_path_start() {
        return Ok(Term::Path(PathExpression::parse(parser)?.unwrap()));
    }

    Err(parser.error_here("expected term"))
}
