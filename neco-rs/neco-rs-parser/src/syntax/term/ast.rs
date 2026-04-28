use super::super::{Item, PathExpression, Pattern};

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
    FieldAccess {
        receiver: Box<Term>,
        field: String,
    },
    StructLiteral {
        path: PathExpression,
        fields: Vec<StructLiteralField>,
    },
    Reference {
        referent: Box<Term>,
        exclusive: bool,
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
    LetRef(LetRefStatement),
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
    pub else_branch: Option<ElseBranch>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElseBranch {
    Block(Block),
    If(Box<IfStatement>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopStatement {
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetStatement {
    pub binder: BindingPattern,
    pub ty: Box<Term>,
    pub operator: LetOperator,
    pub value: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetRefStatement {
    pub reference: String,
    pub exclusive: bool,
    pub ty: Box<Term>,
    pub source: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindingPattern {
    Name(String),
    Wildcard,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructLiteralField {
    pub name: String,
    pub value: Term,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct TermParseOption {
    pub stop_at_left_brace: bool,
    pub stop_at_match_arm_boundary: bool,
}
