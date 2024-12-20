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
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
    pub closing_paren: Token,
}

#[derive(Debug)]
pub struct GetExpression {
    pub object: Box<Expression>,
    pub name: Token,
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
pub struct SuperExpression {
    pub keyword: Token,
    pub method: Token,
}

#[derive(Debug)]
pub struct SetExpression {
    pub object: Box<Expression>,
    pub name: Token,
    pub value: Box<Expression>,
}

#[derive(Debug)]
pub struct ThisExpression {
    pub keyword: Token,
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
    Call(CallExpression),
    Get(GetExpression),
    Grouping(GroupingExpression),
    Literal(Literal),
    Logical(LogicalExpression),
    Set(SetExpression),
    Super(SuperExpression),
    This(ThisExpression),
    Unary(UnaryExpression),
    Variable(Variable),
}

#[derive(Debug)]
pub struct ClassDeclaration {
    pub name: Token,
    pub superclass: Option<Token>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub name: Token,
    pub parameters: Vec<Token>,
    pub body: BlockStatement,
}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub name: Token,
    pub initializer: Option<Expression>,
}

#[derive(Debug)]
pub enum Declaration {
    Class(ClassDeclaration),
    Function(FunctionDeclaration),
    Variable(VariableDeclaration),
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
pub struct ForStatement {
    pub initializer: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug)]
pub struct PrintStatement {
    pub expr: Expression,
}

#[derive(Debug)]
pub struct ReturnStatement {
    pub expr: Option<Expression>,
}

#[derive(Debug)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Assert(AssertStatement),
    Block(BlockStatement),
    Decl(Declaration),
    Expr(ExpressionStatement),
    For(ForStatement),
    If(IfStatement),
    Print(PrintStatement),
    Return(ReturnStatement),
    While(WhileStatement),
}

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}
