use crate::token::Token;

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub struct BinaryExpression {
    pub left_expr: Box<Expression>,
    pub op: Token,
    pub right_expr: Box<Expression>,
}

#[derive(Debug)]
pub struct GroupingExpression {
    pub lparen: Token,
    pub expr: Box<Expression>,
    pub rparen: Token,
}

#[derive(Debug)]
pub struct LogicalExpression {
    pub left_expr: Box<Expression>,
    pub op: Token,
    pub right_expr: Box<Expression>,
}

#[derive(Debug)]
pub struct UnaryExpression {
    pub op: Token,
    pub expr: Box<Expression>,
}

#[derive(Debug)]
pub enum Expression {
    Binary(BinaryExpression),
    Grouping(GroupingExpression),
    Literal(Literal),
    Logical(LogicalExpression),
    Unary(UnaryExpression),
}

#[derive(Debug)]
pub struct AssertStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub enum Statement {
    Assert(AssertStatement),
    Print(PrintStatement),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

