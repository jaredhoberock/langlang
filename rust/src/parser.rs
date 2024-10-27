use crate::syntax::*;
use crate::token::*;

extern crate restore_macros;
use restore_macros::restore_self_on_err;

pub struct ParseError {
    message: String,
    remaining_len: usize,
    error_token: Option<Token>,
}

impl ParseError {
    pub fn new(message: String, remaining: &[Token]) -> Self {
        ParseError {
            message,
            remaining_len: remaining.len(),
            error_token: if remaining.is_empty() {
                None
            } else {
                Some(remaining[0].clone())
            },
        }
    }

    // This returns a closure that takes a ParseError and returns a ParseError whose
    // message has been formatted with the given fmt string
    pub fn format_message(fmt: &'static str) -> impl FnOnce(ParseError) -> ParseError {
        |e| ParseError {
            // format the message of e using the fmt string
            message: fmt.replace("{}", &e.message),
            // preserve the other details of the original ParseError
            remaining_len: e.remaining_len,
            error_token: e.error_token,
        }
    }

    // Take the error with the least remaining input (most progress)
    pub fn combine(errors: Vec<ParseError>) -> ParseError {
        errors.into_iter().min_by_key(|e| e.remaining_len).unwrap()
    }
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let message = match &self.error_token {
            None => format!("at end of input: {}", self.message),
            Some(tok) => format!("at {}: '{}': {}", tok.location, tok.lexeme, self.message),
        };
        write!(f, "{}", message)
    }
}

#[derive(Clone)]
struct Parser<'a> {
    remaining: &'a [Token],
}

impl<'a> Parser<'a> {
    fn new(remaining: &'a [Token]) -> Parser<'a> {
        Parser { remaining }
    }

    // almost all of the methods below use restore_self_on_err
    // to reset the state of the Parser (i.e., the remaining unparsed input)
    // back to its original state upon entering the method when
    // these methods fail a parse
    //
    // this allows us to try to parse one alternative, automatically backtrack,
    // try the next alternative, etc.

    #[restore_self_on_err]
    fn token(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.remaining.is_empty() || self.remaining[0].kind != kind {
            return Err(ParseError::new(
                format!("Expected '{}'", kind),
                self.remaining,
            ));
        }

        let result = self.remaining[0].clone();
        self.remaining = &self.remaining[1..];
        Ok(result)
    }

    #[restore_self_on_err]
    fn either_token(&mut self, kind0: TokenKind, kind1: TokenKind) -> Result<Token, ParseError> {
        let tok0 = self.token(kind0);
        if tok0.is_ok() {
            return tok0;
        }

        let tok1 = self.token(kind1);
        if tok1.is_ok() {
            return tok1;
        }

        let errors = vec![tok0.unwrap_err(), tok1.unwrap_err()];

        Err(ParseError::combine(errors))
    }

    #[restore_self_on_err]
    fn any_comparison_operator(&mut self) -> Result<Token, ParseError> {
        let gt = self.token(TokenKind::Greater);
        if gt.is_ok() {
            return gt;
        }

        let gte = self.token(TokenKind::GreaterEqual);
        if gte.is_ok() {
            return gte;
        }

        let lt = self.token(TokenKind::Less);
        if lt.is_ok() {
            return lt;
        }

        let lte = self.token(TokenKind::LessEqual);
        if lte.is_ok() {
            return lte;
        }

        Err(ParseError::new("Expected comparison operator".to_string(), self.remaining))
    }

    fn identifier(&mut self) -> Result<Token, ParseError> {
        self.token(TokenKind::Identifier)
    }

    fn variable(&mut self) -> Result<Variable, ParseError> {
        self.identifier().map(|name| Variable { name })
    }

    fn number_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::Number)
            .map(|token| Literal::Number(token.literal.unwrap().as_number()))
    }

    fn string_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::String)
            .map(|token| Literal::String(token.literal.unwrap().as_string()))
    }

    fn true_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::True).map(|_| Literal::Bool(true))
    }

    fn false_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::False).map(|_| Literal::Bool(false))
    }

    fn nil_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::Nil).map(|_| Literal::Nil)
    }

    // grouping_expression := '(' expression ')'
    #[restore_self_on_err]
    fn grouping_expression(&mut self) -> Result<GroupingExpression, ParseError> {
        Ok(GroupingExpression {
            lparen: self.token(TokenKind::LeftParen)?,
            expr: Box::new(self.expression()?),
            rparen: self.token(TokenKind::RightParen)?,
        })
    }

    // literal := number_literal | string_literal | 'true' | 'false' | 'nil'
    #[restore_self_on_err]
    fn literal(&mut self) -> Result<Literal, ParseError> {
        let number = self.number_literal();
        if number.is_ok() {
            return number;
        }

        let string = self.string_literal();
        if string.is_ok() {
            return string;
        }

        let true_ = self.true_literal();
        if true_.is_ok() {
            return true_;
        }

        let false_ = self.false_literal();
        if false_.is_ok() {
            return false_;
        }

        let nil = self.nil_literal();
        if nil.is_ok() {
            return nil;
        }

        let errors = vec![
            number.unwrap_err(),
            string.unwrap_err(),
            true_.unwrap_err(),
            false_.unwrap_err(),
            nil.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // primary_expression := literal | grouping_expression | variable
    #[restore_self_on_err]
    fn primary_expression(&mut self) -> Result<Expression, ParseError> {
        let literal = self.literal();
        if literal.is_ok() {
            return literal.map(Expression::Literal);
        }

        let grouping = self.grouping_expression();
        if grouping.is_ok() {
            return grouping.map(Expression::Grouping);
        }

        let var = self.variable();
        if var.is_ok() {
            return var.map(Expression::Variable);
        }

        let errors = vec![
            literal.unwrap_err(),
            grouping.unwrap_err(),
            var.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // arguments := expression ( "," expression )*
    #[restore_self_on_err]
    fn arguments(&mut self) -> Result<Vec<Expression>, ParseError> {
        let mut result = Vec::new();

        loop {
            if result.len() == 255 {
                return Err(ParseError::new(
                    "Can't have more than 255 arguments".to_string(),
                    self.remaining,
                ));
            }

            let err_fmt_str = if result.len() > 0 {
                "{} after ',' in argument list"
            } else {
                "{}"
            };

            let expr = self
                .expression()
                .map_err(ParseError::format_message(err_fmt_str))?;

            result.push(expr);

            // look for a comma to indicate another argument
            if let Err(_) = self.token(TokenKind::Comma) {
                break;
            }
        }

        Ok(result)
    }

    // call := primary_expression ( "(" arguments? ")" )*
    #[restore_self_on_err]
    fn call(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.primary_expression()?;

        loop {
            // look for an open paren
            if self.token(TokenKind::LeftParen).is_err() {
                break;
            }

            if let Ok(closing_paren) = self.token(TokenKind::RightParen) {
                let call_expr = CallExpression {
                    callee: Box::new(result),
                    arguments: Vec::new(),
                    closing_paren,
                };
                result = Expression::Call(call_expr);
                break;
            }

            let arguments = self
                .arguments()
                .map_err(ParseError::format_message("{} after '('"))?;

            let closing_paren = self
                .token(TokenKind::RightParen)
                .map_err(ParseError::format_message("{} after arguments"))?;

            let call_expr = CallExpression {
                callee: Box::new(result),
                arguments,
                closing_paren,
            };
            result = Expression::Call(call_expr);
        }

        Ok(result)
    }

    // unary := ( "!" | "-" ) | call
    #[restore_self_on_err]
    fn unary(&mut self) -> Result<Expression, ParseError> {
        if let Ok(op) = self.either_token(TokenKind::Bang, TokenKind::Minus) {
            let expr = UnaryExpression {
                op: op,
                expr: Box::new(self.unary()?),
            };
            Ok(Expression::Unary(expr))
        } else {
            self.call()
        }
    }

    // factor := unary ( ( "/" | "*" ) ) unary )*
    #[restore_self_on_err]
    fn factor(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.unary()?;

        while let Ok(op) = self.either_token(TokenKind::Slash, TokenKind::Star) {
            // parse the rhs
            let right_expr = self.unary()?;

            result = Expression::Binary(BinaryExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // term := factor ( ( "-" | "+" ) factor )*
    #[restore_self_on_err]
    fn term(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.factor()?;

        while let Ok(op) = self.either_token(TokenKind::Minus, TokenKind::Plus) {
            // parse the rhs
            let right_expr = self.factor()?;

            result = Expression::Binary(BinaryExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
    fn comparison(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.term()?;

        while let Ok(op) = self.any_comparison_operator() {
            // parse the rhs
            let right_expr = self.term()?;

            result = Expression::Binary(BinaryExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // equality := comparison ( ( "!=" | "==" ) comparison )*
    #[restore_self_on_err]
    fn equality(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.comparison()?;

        while let Ok(op) = self.either_token(TokenKind::BangEqual, TokenKind::EqualEqual) {
            // parse the rhs
            let right_expr = self.comparison()?;

            result = Expression::Binary(BinaryExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // logical_and := equality ( "and" equality)*
    #[restore_self_on_err]
    fn logical_and(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.equality()?;

        while let Ok(op) = self.token(TokenKind::And) {
            // parse the rhs
            let right_expr = self.equality()?;

            result = Expression::Logical(LogicalExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // logical_or := logical_and ( "or" logical_and)*
    #[restore_self_on_err]
    fn logical_or(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.logical_and()?;

        while let Ok(op) = self.token(TokenKind::Or) {
            // parse the rhs
            let right_expr = self.logical_and()?;

            result = Expression::Logical(LogicalExpression {
                left_expr: Box::new(result),
                op,
                right_expr: Box::new(right_expr),
            })
        }

        Ok(result)
    }

    // assignment := logical_or | variable "=" assignment
    #[restore_self_on_err]
    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.logical_or()?;

        if let Ok(_) = self.token(TokenKind::Equal) {
            if let Expression::Variable(var) = result {
                // parse the rhs
                let right_expr = self.assignment()?;

                result = Expression::Assignment(AssignmentExpression {
                    var,
                    expr: Box::new(right_expr),
                });
            } else {
                return Err(ParseError::new(
                    "Invalid assignment target".to_string(),
                    self.remaining,
                ));
            }
        }

        Ok(result)
    }

    // expression := assignment
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    // variable_declaration := "var" identifier ( "=" expression )? ";"
    #[restore_self_on_err]
    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let _var = self.token(TokenKind::Var)?;
        let name = self.identifier()?;

        let initializer = if self.token(TokenKind::Equal).is_ok() {
            Some(self.expression()?)
        } else {
            None
        };

        let _semi = self.token(TokenKind::Semicolon)?;

        Ok(Statement::VarDecl(VariableDeclaration {
            name,
            initializer,
        }))
    }

    // assert_statement := "assert" expression ";"
    #[restore_self_on_err]
    fn assert_statement(&mut self) -> Result<Statement, ParseError> {
        let _assert = self.token(TokenKind::Assert)?;
        let expr = self.expression()?;
        let _semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Assert(AssertStatement { expr }))
    }

    // block_statement := "{" ( declaration )* "}"
    #[restore_self_on_err]
    fn block_statement(&mut self) -> Result<BlockStatement, ParseError> {
        let _lbrace = self
            .token(TokenKind::LeftBrace)
            .map_err(ParseError::format_message("{} before block"))?;

        let mut statements = Vec::new();

        loop {
            if self.token(TokenKind::RightBrace).is_ok() {
                break;
            }

            let stmt = self.declaration()?;
            statements.push(stmt);
        }

        Ok(BlockStatement { statements })
    }

    // expression_statement := expression ";"
    #[restore_self_on_err]
    fn expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.expression()?;
        let _semi = self.token(TokenKind::Semicolon)?;
        return Ok(Statement::Expr(ExpressionStatement { expr }));
    }

    // if_statement := "if" "(" expression ")" statement ( "else" statement )?
    #[restore_self_on_err]
    fn if_statement(&mut self) -> Result<Statement, ParseError> {
        let _if = self.token(TokenKind::If)?;
        let _lparen = self.token(TokenKind::LeftParen)?;
        let condition = self.expression()?;
        let _rparen = self.token(TokenKind::RightParen)?;
        let then_branch = Box::new(self.statement()?);

        let mut else_branch = None;
        if let Ok(_else) = self.token(TokenKind::Else) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Statement::If(IfStatement{condition, then_branch, else_branch}))
    }

    // print_statement := "print" expression ";"
    #[restore_self_on_err]
    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let _print = self.token(TokenKind::Print)?;
        let expr = self.expression()?;
        let _semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Print(PrintStatement { expr }))
    }

    // return_statement := "return" expression? ";"
    #[restore_self_on_err]
    fn return_statement(&mut self) -> Result<Statement, ParseError> {
        self.token(TokenKind::Return)?;
        let mut expr = None;
        if let Ok(e) = self.expression() {
            expr = Some(e);
        }
        self.token(TokenKind::Semicolon)?;
        Ok(Statement::Return(ReturnStatement { expr }))
    }

    // while_statement := "while" "(" condition ")" statement
    fn while_statement(&mut self) -> Result<Statement, ParseError> {
        let _while = self.token(TokenKind::While)?;
        let _lparen = self.token(TokenKind::LeftParen)?;
        let condition = self.expression()?;
        let _rparen = self.token(TokenKind::RightParen)?;
        let body = Box::new(self.statement()?);
        Ok(Statement::While(WhileStatement { condition, body }))
    }

    // statement := assert_statement | block_statement | expression_statement | if_statement | print_statement | return_statement | while_statement
    #[restore_self_on_err]
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let assert = self.assert_statement();
        if assert.is_ok() {
            return assert;
        }

        let block = self.block_statement();
        if block.is_ok() {
            return block.map(Statement::Block);
        }

        let expr_stmt = self.expression_statement();
        if expr_stmt.is_ok() {
            return expr_stmt;
        }

        let if_stmt = self.if_statement();
        if if_stmt.is_ok() {
            return if_stmt;
        }

        let print = self.print_statement();
        if print.is_ok() {
            return print;
        }

        let ret = self.return_statement();
        if ret.is_ok() {
            return ret;
        }

        let while_ = self.while_statement();
        if while_.is_ok() {
            return while_;
        }

        let errors = vec![
            assert.unwrap_err(),
            block.unwrap_err(),
            expr_stmt.unwrap_err(),
            if_stmt.unwrap_err(),
            print.unwrap_err(),
            ret.unwrap_err(),
            while_.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // parameters := identifier ( "," identifier )*
    #[restore_self_on_err]
    fn parameters(&mut self) -> Result<Vec<Token>, ParseError> {
        let mut params = vec![self.identifier()?];

        while self.token(TokenKind::Comma).is_ok() {
            let param = self
                .identifier()
                .map_err(ParseError::format_message("{} in function parameter list"))?;
            params.push(param);
        }

        Ok(params)
    }

    // function_declaration := "fun" identifier "(" parameters? ")" block_statement
    #[restore_self_on_err]
    fn function_declaration(&mut self) -> Result<Statement, ParseError> {
        let _fun = self
            .token(TokenKind::Fun)
            .map_err(ParseError::format_message("{} before function name"))?;
        let name = self.identifier()?;
        let _lparen = self.token(TokenKind::LeftParen)?;

        let mut parameters = Vec::new();
        if let Ok(params) = self.parameters() {
            parameters = params;
        }

        let _rparen = self.token(TokenKind::RightParen)?;
        let body = self.block_statement()?;

        Ok(Statement::FunDecl(FunctionDeclaration {
            name,
            parameters,
            body,
        }))
    }

    // declaration := function_declaration | variable_declaration | statement
    #[restore_self_on_err]
    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let fun_decl = self.function_declaration();
        if fun_decl.is_ok() {
            return fun_decl;
        }

        let var_decl = self.variable_declaration();
        if var_decl.is_ok() {
            return var_decl;
        }

        let stmt = self.statement();
        if stmt.is_ok() {
            return stmt;
        }

        let errors = vec![
            fun_decl.unwrap_err(),
            var_decl.unwrap_err(),
            stmt.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // program := declaration* EOF
    #[restore_self_on_err]
    fn program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();

        loop {
            if self.token(TokenKind::Eof).is_ok() {
                break;
            }

            match self.declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(error) => return Err(error),
            }
        }

        Ok(Program { statements: stmts })
    }
}

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.program()
}
