use std::collections::HashMap;
use std::ptr::NonNull;

use crate::syntax::*;
use crate::token::Token;

pub struct NameResolver {
    // maps each resolved name to the scope in which its referent is defined
    symbol_table: HashMap<NonNull<Variable>, usize>,

    // a scope maps a name to whether or not it has been defined
    scopes: Vec<HashMap<String, bool>>,
}

impl NameResolver {
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    pub fn lookup(&self, var: &Variable) -> Result<usize, String> {
        match self.symbol_table.get(&NonNull::from(var)) {
            Some(&depth) => Ok(depth),
            None => Err(format!(
                "Internal error: variable '{}' was not resolved",
                var.name.lexeme
            )),
        }
    }

    fn with_new_scope<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, String>,
    ) -> Result<T, String> {
        self.scopes.push(HashMap::new());
        let result = f(self);
        self.scopes.pop();
        result
    }

    fn declare(&mut self, name: &Token) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err("Cannot declare variable outside of a scope".to_string());
        }

        if self.scopes.last().unwrap().contains_key(&name.lexeme) {
            return Err(format!(
                "Variable '{}' is already declared in this scope",
                name.lexeme
            ));
        }

        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err("Cannot define variable outside of a scope".to_string());
        }

        let scope = self.scopes.last_mut().unwrap();

        if !scope.contains_key(&name.lexeme) {
            return Err(format!(
                "Cannot define variable '{}' before declaration",
                name.lexeme
            ));
        }

        scope.insert(name.lexeme.clone(), true);
        Ok(())
    }

    fn resolve_variable(&mut self, variable: &Variable) -> Result<(), String> {
        if self.scopes.is_empty() {
            return Err("Cannot resolve variable outside of a scope".to_string());
        }

        // look through each scope, starting at the top of
        // the stack for a declaration for name, and note
        // the location of its scope if found
        for i in (0..self.scopes.len()).rev() {
            if self.scopes[i].contains_key(&variable.name.lexeme) {
                // we record the distance that we need to "climb" from
                // the innermost scope to find the name's referent
                self.symbol_table
                    .insert(NonNull::from(variable), self.scopes.len() - 1 - i);
                return Ok(());
            }
        }

        Err(format!(
            "Name resolution error: Variable '{}' is undefined",
            variable.name.lexeme
        ))
    }

    fn resolve_literal(&mut self, _lit: &Literal) -> Result<(), String> {
        Ok(())
    }

    fn resolve_unary_expression(&mut self, expr: &UnaryExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_binary_expression(&mut self, expr: &BinaryExpression) -> Result<(), String> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_grouping_expression(&mut self, expr: &GroupingExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)
    }

    fn resolve_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<(), String> {
        self.resolve_expression(&expr.expr)?;
        self.resolve_variable(&expr.var)
    }

    fn resolve_logical_expression(&mut self, expr: &LogicalExpression) -> Result<(), String> {
        self.resolve_expression(&expr.left_expr)?;
        self.resolve_expression(&expr.right_expr)
    }

    fn resolve_call_expression(&mut self, call: &CallExpression) -> Result<(), String> {
        self.resolve_expression(&call.callee)?;
        for arg in &call.arguments {
            self.resolve_expression(arg)?;
        }
        Ok(())
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Result<(), String> {
        match expr {
            Expression::Assignment(a) => self.resolve_assignment_expression(a),
            Expression::Binary(b) => self.resolve_binary_expression(b),
            Expression::Call(c) => self.resolve_call_expression(c),
            Expression::Grouping(g) => self.resolve_grouping_expression(g),
            Expression::Literal(l) => self.resolve_literal(l),
            Expression::Logical(l) => self.resolve_logical_expression(l),
            Expression::Unary(u) => self.resolve_unary_expression(u),
            Expression::Variable(v) => self.resolve_variable(v),
        }
    }

    fn resolve_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_return_statement(&mut self, stmt: &ReturnStatement) -> Result<(), String> {
        if let Some(expr) = &stmt.expr {
            self.resolve_expression(expr)
        } else {
            Ok(())
        }
    }

    fn resolve_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), String> {
        self.resolve_expression(&stmt.expr)
    }

    fn resolve_block_statement(&mut self, block: &BlockStatement) -> Result<(), String> {
        self.with_new_scope(|slf| {
            for stmt in &block.statements {
                slf.resolve_statement(stmt)?;
            }
            Ok(())
        })
    }

    fn resolve_function_declaration(&mut self, decl: &FunctionDeclaration) -> Result<(), String> {
        self.declare(&decl.name)?;
        self.define(&decl.name)?;

        self.with_new_scope(|slf| {
            for param in &decl.parameters {
                slf.declare(param)?;
                slf.define(param)?;
            }
            slf.resolve_block_statement(&decl.body)
        })
    }

    fn resolve_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), String> {
        self.declare(&decl.name)?;
        if let Some(init) = &decl.initializer {
            self.resolve_expression(init)?;
        }
        self.define(&decl.name)
    }

    fn resolve_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Assert(a) => self.resolve_assert_statement(a),
            Statement::Block(b) => self.resolve_block_statement(b),
            Statement::Expr(e) => self.resolve_expression_statement(e),
            Statement::FunDecl(f) => self.resolve_function_declaration(f),
            Statement::Print(p) => self.resolve_print_statement(p),
            Statement::Return(r) => self.resolve_return_statement(r),
            Statement::VarDecl(v) => self.resolve_variable_declaration(v),
        }
    }

    pub fn resolve_program(&mut self, prog: &Program) -> Result<(), String> {
        if !self.scopes.is_empty() {
            return Err("Internal error: scopes should be empty at start of program".to_string());
        }

        self.with_new_scope(|slf| {
            for stmt in &prog.statements {
                slf.resolve_statement(stmt)?;
            }
            Ok(())
        })?;

        if !self.scopes.is_empty() {
            return Err("Internal error: scopes should be empty at end of program".to_string());
        }

        Ok(())
    }
}
