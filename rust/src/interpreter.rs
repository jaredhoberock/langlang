use crate::syntax::*;
use crate::token::*;

#[derive(Debug, PartialEq)]
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

struct Environment {
}

impl Environment {
    fn new() -> Self {
        Environment {}
    }
}

pub struct Interpreter {
    global_env: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            global_env: Environment::new(),
        }
    }

    fn interpret_literal(&self, _env: &Environment, lit: &Literal) -> Result<Value, String> {
        Ok(match lit {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_grouping_expression(&self, env: &Environment, expr: &GroupingExpression) -> Result<Value, String> {
        self.interpret_expression(env, &*expr.expr)
    }

    fn interpret_logical_expression(&self, env: &Environment, expr: &LogicalExpression) -> Result<Value, String> {
        let left = self.interpret_expression(env, &*expr.left_expr)?;
        match expr.op.kind {
            TokenKind::Or => if left.as_bool() { return Ok(left) },
            TokenKind::And => if !left.as_bool() { return Ok(left) },
            _ => return Err(format!("Unexpected '{}' in logical expression", expr.op.lexeme)) 
        };
        return self.interpret_expression(env, &*expr.right_expr);
    }

    fn interpret_binary_expression(&self, env: &Environment, expr: &BinaryExpression) -> Result<Value, String> {
        let lhs = self.interpret_expression(env, &*expr.left_expr)?;
        let rhs = self.interpret_expression(env, &*expr.right_expr)?;
        lhs.evaluate_binary_operation(&expr.op.kind, &rhs)
    }

    fn interpret_unary_expression(&self, env: &Environment, expr: &UnaryExpression) -> Result<Value, String> {
        let value = self.interpret_expression(env, &*expr.expr)?;
        match expr.op.kind {
            TokenKind::Bang => Ok(Value::Bool(!value.as_bool())),
            TokenKind::Minus => Ok(Value::Number(-value.as_f64()?)),
            _ => Err("Bad operator in unary_expression".to_string()),
        }
    }

    fn interpret_expression(&self, env: &Environment, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Binary(expr) => self.interpret_binary_expression(env, expr),
            Expression::Grouping(g) => self.interpret_grouping_expression(env, g),
            Expression::Literal(l) => self.interpret_literal(env, l),
            Expression::Logical(expr) => self.interpret_logical_expression(env, expr),
            Expression::Unary(expr) => self.interpret_unary_expression(env, expr),
        }
    }

    fn interpret_assert_statement(&self, env: &Environment, stmt: &AssertStatement) -> Result<(), String> {
        let val = self.interpret_expression(env, &stmt.expr)?;
        if !val.as_bool() {
            return Err("assert failed".to_string());
        }
        Ok(())
    }

    fn interpret_print_statement(&self, env: &Environment, stmt: &PrintStatement) -> Result<(), String> {
        let val = self.interpret_expression(env, &stmt.expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_statement(&self, env: &Environment, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::Assert(stmt) => self.interpret_assert_statement(env, stmt),
            Statement::Print(stmt) => self.interpret_print_statement(env, stmt),
        }
    }

    pub fn interpret_program(&self, prog: &Program) -> Result<(), String> {
        for stmt in &prog.statements {
            self.interpret_statement(&self.global_env, stmt)?
        }
        Ok(())
    }
}

