use crate::syntax::*;

#[derive(Debug)]
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

    fn interpret_expression(&self, env: &Environment, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Literal(l) => self.interpret_literal(env, l),
            Expression::Grouping(g) => self.interpret_grouping_expression(env, g),
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

