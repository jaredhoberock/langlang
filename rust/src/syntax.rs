use crate::token::Token;

#[derive(Debug)]
pub enum Literal {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

#[derive(Debug)]
pub struct Variable {
    pub name: Token,
}

#[derive(Debug)]
pub struct AssignmentExpression {
    pub var: Variable,
    pub expr: Box<Expression>,
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
    Assignment(AssignmentExpression),
    Binary(BinaryExpression),
    Grouping(GroupingExpression),
    Literal(Literal),
    Logical(LogicalExpression),
    Unary(UnaryExpression),
    Variable(Variable),
}

#[derive(Debug)]
pub struct AssertStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Token,
    pub initializer: Option<Expression>,
}

#[derive(Debug)]
pub enum Statement {
    Assert(AssertStatement),
    Block(BlockStatement),
    Expr(ExpressionStatement),
    Print(PrintStatement),
    VarDecl(VariableDeclaration),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

