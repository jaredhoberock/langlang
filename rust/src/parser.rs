use crate::token::*;
use crate::syntax::*;

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
            }
        }
    }

    // Take the error with the least remaining input (most progress)
    pub fn combine(errors: Vec<ParseError>) -> ParseError {
        errors.into_iter()
            .min_by_key(|e| e.remaining_len)
            .unwrap()
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

struct Parser<'a> {
    remaining: &'a [Token],
}

impl<'a> Parser<'a> {
    fn new(remaining: &'a [Token]) -> Parser<'a> {
        Parser {
            remaining,
        }
    }

    fn token(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        if self.remaining.is_empty() || self.remaining[0].kind != kind {
            return Err(ParseError::new(format!("Expected '{}'", kind), self.remaining))
        }

        let result = self.remaining[0].clone();
        self.remaining = &self.remaining[1..];
        Ok(result)
    }

    fn either_token(&mut self, kind0: TokenKind, kind1: TokenKind) -> Result<Token, ParseError> {
        let original_remaining = self.remaining;

        let tok0 = self.token(kind0);
        if tok0.is_ok() {
            return tok0;
        }

        let tok1 = self.token(kind1);
        if tok1.is_ok() {
            return tok1
        }

        self.remaining = original_remaining;

        let errors = vec![
            tok0.unwrap_err(),
            tok1.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    fn identifier(&mut self) -> Result<Token, ParseError> {
        self.token(TokenKind::Identifier)
    }

    fn variable(&mut self) -> Result<Variable, ParseError> {
        self.identifier().map(|name| { Variable{ name } })
    }

    fn number_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::Number)
            .map(|token| {
                Literal::Number(token.literal.unwrap().as_number())
            })
    }

    fn string_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::String)
            .map(|token| {
                Literal::String(token.literal.unwrap().as_string())
            })
    }

    fn true_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::True)
            .map(|_| {
                Literal::Bool(true)
            })
    }

    fn false_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::False)
            .map(|_| {
                Literal::Bool(false)
            })
    }

    fn nil_literal(&mut self) -> Result<Literal, ParseError> {
        self.token(TokenKind::Nil)
            .map(|_| {
                Literal::Nil
            })
    }

    // grouping_expression := '(' expression ')'
    fn grouping_expression(&mut self) -> Result<GroupingExpression, ParseError> {
        Ok(GroupingExpression{
            lparen : self.token(TokenKind::LeftParen)?, 
            expr : Box::new(self.expression()?),
            rparen : self.token(TokenKind::RightParen)?
        })
    }

    // literal := number_literal | string_literal | 'true' | 'false' | 'nil'
    fn literal(&mut self) -> Result<Literal, ParseError> {
        // note the original state of remaining
        let original_remaining = self.remaining;

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

        // restore state after failure
        self.remaining = original_remaining;

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
    fn primary_expression(&mut self) -> Result<Expression, ParseError> {
        let original_remaining = self.remaining;

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

        self.remaining = original_remaining;

        let errors = vec![
            literal.unwrap_err(),
            grouping.unwrap_err(),
            var.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // call := primary_expression
    fn call(&mut self) -> Result<Expression, ParseError> {
        self.primary_expression()
    }

    // unary := ( "!" | "-" ) | call
    fn unary(&mut self) -> Result<Expression, ParseError> {
        let original_remaining = self.remaining;

        if let Ok(op) = self.either_token(TokenKind::Bang, TokenKind::Minus) {
            let expr = UnaryExpression{
                op : op,
                expr : Box::new(self.unary()?),
            };
            Ok(Expression::Unary(expr))
        } else {
            self.remaining = original_remaining;
            self.call()
        }
    }

    // factor := unary ( ( "/" | "*" ) ) unary )*
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
        };

        Ok(result)
    }

    // term := factor ( ( "-" | "+" ) factor )*
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
        };

        Ok(result)
    }

    // comparison := term
    fn comparison(&mut self) -> Result<Expression, ParseError> {
        self.term()
    }

    // equality := comparison ( ( "!=" | "==" ) comparison )*
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
        };

        Ok(result)
    }

    // logical_and := equality ( "and" equality)*
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
        };

        Ok(result)
    }

    // logical_or := logical_and ( "or" logical_and)*
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
        };

        Ok(result)
    }

    // assignment := logical_or | variable "=" assignment
    fn assignment(&mut self) -> Result<Expression, ParseError> {
        let mut result = self.logical_or()?;

        let original_remaining = self.remaining;

        if let Ok(_) = self.token(TokenKind::Equal) {
            if let Expression::Variable(var) = result {
                // parse the rhs
                let right_expr = self.assignment()?;

                result = Expression::Assignment(AssignmentExpression {
                    var,
                    expr: Box::new(right_expr),
                });
            }
            else {
                let err = ParseError::new("Invalid assignment target".to_string(), self.remaining);
                self.remaining = original_remaining;
                return Err(err);
            }
        }

        Ok(result)
    }

    // expression := assignment
    fn expression(&mut self) -> Result<Expression, ParseError> {
        self.assignment()
    }

    // assert_statement := "assert" expression ";"
    fn assert_statement(&mut self) -> Result<Statement, ParseError> {
        let _assert = self.token(TokenKind::Assert)?;
        let expr = self.expression()?;
        let _semi = self.token(TokenKind::Semicolon)?;
        return Ok(Statement::Assert(AssertStatement{expr}));
    }

    // print_statement := "print" expression ";"
    fn print_statement(&mut self) -> Result<Statement, ParseError> {
        let _print = self.token(TokenKind::Print)?;
        let expr = self.expression()?;
        let _semi = self.token(TokenKind::Semicolon)?;
        Ok(Statement::Print(PrintStatement{expr}))
    }

    // statement := assert_statement | print_statement
    fn statement(&mut self) -> Result<Statement, ParseError> {
        let original_remaining = self.remaining;

        let assert = self.assert_statement();
        if assert.is_ok() {
            return assert;
        }

        let print = self.print_statement();
        if print.is_ok() {
            return print;
        }

        // restore state after failure
        self.remaining = original_remaining;

        let errors = vec![
            assert.unwrap_err(),
            print.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // variable_declaration := "var" identifier ( "=" expression )? ";"
    fn variable_declaration(&mut self) -> Result<Statement, ParseError> {
        let _var = self.token(TokenKind::Var)?;
        let name = self.identifier()?;

        let initializer = if self.token(TokenKind::Equal).is_ok() {
            Some(self.expression()?)
        } else {
            None
        };

        let _semi = self.token(TokenKind::Semicolon)?;

        Ok(Statement::VarDecl(VariableDeclaration{name, initializer}))
    }

    // declaration := variable_declaration | statement
    fn declaration(&mut self) -> Result<Statement, ParseError> {
        let original_remaining = self.remaining;

        let var_decl = self.variable_declaration();
        if var_decl.is_ok() {
            return var_decl;
        }

        let stmt = self.statement();
        if stmt.is_ok() {
            return stmt;
        }

        self.remaining = original_remaining;

        let errors = vec![
            var_decl.unwrap_err(),
            stmt.unwrap_err(),
        ];

        Err(ParseError::combine(errors))
    }

    // program := declaration* EOF
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

        Ok(Program{statements: stmts})
    }
}

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.program()
}

