use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::syntax::*;
use crate::token::*;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    // XXX or Callable, eventually
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    s[..s.len()-2].to_string()
                } else {
                    s
                }
            },
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
        }
    }

    pub fn as_f64(&self) -> Result<f64,String> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err("Value is not a number".to_string()),
        }
    }

    pub fn evaluate_binary_operation(&self, op: &TokenKind, rhs: &Self) -> Result<Self,String> {
        use Value::*;

        match op {
            TokenKind::BangEqual => Ok(Bool(self != rhs)),
            TokenKind::EqualEqual => Ok(Bool(self == rhs)),
            TokenKind::Greater => Ok(Bool(self.as_f64()? > rhs.as_f64()?)),
            TokenKind::GreaterEqual => Ok(Bool(self.as_f64()? >= rhs.as_f64()?)),
            TokenKind::Less => Ok(Bool(self.as_f64()? < rhs.as_f64()?)),
            TokenKind::LessEqual => Ok(Bool(self.as_f64()? <= rhs.as_f64()?)),
            TokenKind::Minus => Ok(Number(self.as_f64()? - rhs.as_f64()?)),
            TokenKind::Plus => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 + n2)),
                (String(s1), String(s2)) => Ok(String(s1.clone() + &s2)),
                (_, _) => Err("Operands must be two numbers or two strings.".to_string()),
            },
            TokenKind::Slash => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 / n2)),
                (_,_) => Err("Operands must be two numbers.".to_string()),
            },
            TokenKind::Star => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
                (_,_) => Err("Operands must be two numbers.".to_string()),
            },
            _ => Err("Unexpected operator in binary operation".to_string()),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.as_string())
    }
}

#[derive(Debug, Clone)]
struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    fn get(&self, name: &Token) -> Result<Value,String> {
        if let Some(value) = self.values.get(&name.lexeme) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(&name)
        } else {
            Err(format!("Undefined variable '{}'.", &name.lexeme))
        }
    }

    fn put(&mut self, name: &Token, value: &Value) -> Result<(),String> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        }
        else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().put(name, value)
        }
        else {
            Err(format!("Undefined variable '{}'.", &name.lexeme))
        }
    }

    fn define(&mut self, name: &Token, value: &Value) -> Result<(),String> {
        if self.values.contains_key(&name.lexeme) {
            Err(format!("Variable '{}' is already defined.", &name.lexeme))
        } else {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        }
    }
}

pub struct Interpreter {
    current_environment : Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global_environment = Environment::new();
        Interpreter {
            current_environment : Rc::new(RefCell::new(global_environment)),
        }
    }

    pub fn push_environment(&mut self) {
        let new_env = Environment::new_with_enclosing(self.current_environment.clone());
        self.current_environment = Rc::new(RefCell::new(new_env));
    }

    pub fn pop_environment(&mut self) {
        let enclosing = {
            let current = self.current_environment.borrow();
            current.enclosing.clone()
        };
        self.current_environment = enclosing.expect("Can't pop global environment");
    }

    fn interpret_literal(&self, lit: &Literal) -> Result<Value, String> {
        Ok(match lit {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_grouping_expression(&mut self, expr: &GroupingExpression) -> Result<Value, String> {
        self.interpret_expression(&*expr.expr)
    }

    fn interpret_logical_expression(&mut self, expr: &LogicalExpression) -> Result<Value, String> {
        let left = self.interpret_expression(&*expr.left_expr)?;
        match expr.op.kind {
            TokenKind::Or => if left.as_bool() { return Ok(left) },
            TokenKind::And => if !left.as_bool() { return Ok(left) },
            _ => return Err(format!("Unexpected '{}' in logical expression", expr.op.lexeme)) 
        };
        return self.interpret_expression(&*expr.right_expr);
    }

    fn interpret_binary_expression(&mut self, expr: &BinaryExpression) -> Result<Value, String> {
        let lhs = self.interpret_expression(&*expr.left_expr)?;
        let rhs = self.interpret_expression(&*expr.right_expr)?;
        lhs.evaluate_binary_operation(&expr.op.kind, &rhs)
    }

    fn interpret_unary_expression(&mut self, expr: &UnaryExpression) -> Result<Value, String> {
        let value = self.interpret_expression(&*expr.expr)?;
        match expr.op.kind {
            TokenKind::Bang => Ok(Value::Bool(!value.as_bool())),
            TokenKind::Minus => Ok(Value::Number(-value.as_f64()?)),
            _ => Err("Bad operator in unary_expression".to_string()),
        }
    }

    fn interpret_variable(&mut self, var: &Variable) -> Result<Value, String> {
        self.current_environment.borrow().get(&var.name)
    }

    fn interpret_assignment_expression(&mut self, expr: &AssignmentExpression) -> Result<Value, String> {
        let value = self.interpret_expression(&*expr.expr)?;
        self.current_environment.borrow_mut().put(&expr.var.name, &value)?;
        Ok(value)
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Assignment(expr) => self.interpret_assignment_expression(expr),
            Expression::Binary(expr) => self.interpret_binary_expression(expr),
            Expression::Grouping(g) => self.interpret_grouping_expression(g),
            Expression::Literal(l) => self.interpret_literal(l),
            Expression::Logical(expr) => self.interpret_logical_expression(expr),
            Expression::Unary(expr) => self.interpret_unary_expression(expr),
            Expression::Variable(var) => self.interpret_variable(var),
        }
    }

    fn interpret_assert_statement(&mut self, stmt: &AssertStatement) -> Result<(), String> {
        let val = self.interpret_expression(&stmt.expr)?;
        if !val.as_bool() {
            return Err("assert failed".to_string());
        }
        Ok(())
    }

    fn interpret_block_statement(&mut self, block: &BlockStatement) -> Result<(), String> {
        self.push_environment();

        let mut result = Ok(());

        for stmt in &block.statements {
            if let Err(e) = self.interpret_statement(stmt) {
                result = Err(e);
                break;
            }
        };

        self.pop_environment();

        result
    }

    fn interpret_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), String> {
        self.interpret_expression(&stmt.expr)?;
        Ok(())
    }

    fn interpret_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), String> {
        let val = self.interpret_expression(&stmt.expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), String> {
        let value = match &decl.initializer {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        self.current_environment.borrow_mut().define(&decl.name, &value)
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Assert(stmt) => self.interpret_assert_statement(stmt),
            Statement::Block(stmt) => self.interpret_block_statement(stmt),
            Statement::Expr(stmt) => self.interpret_expression_statement(stmt),
            Statement::Print(stmt) => self.interpret_print_statement(stmt),
            Statement::VarDecl(decl) => self.interpret_variable_declaration(decl),
        }
    }

    pub fn interpret_program(&mut self, prog: &Program) -> Result<(), String> {
        for stmt in &prog.statements {
            self.interpret_statement(stmt)?
        }
        Ok(())
    }
}

