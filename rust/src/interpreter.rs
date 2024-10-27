use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::ControlFlow;
use std::rc::Rc;

use crate::name_resolver::NameResolver;
use crate::syntax::*;
use crate::token::*;

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Callable(Callable),
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Value {
    fn as_bool(&self) -> bool {
        match self {
            Value::Bool(b) => *b,
            Value::Nil => false,
            _ => true,
        }
    }

    fn as_string(&self) -> String {
        match self {
            Value::Callable(c) => c.to_string(),
            Value::Number(n) => {
                let s = n.to_string();
                if s.ends_with(".0") {
                    s[..s.len() - 2].to_string()
                } else {
                    s
                }
            }
            Value::String(s) => s.clone(),
            Value::Bool(b) => b.to_string(),
            Value::Nil => "nil".to_string(),
        }
    }

    fn as_f64(&self) -> Result<f64, String> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err("Value is not a number".to_string()),
        }
    }

    fn evaluate_binary_operation(&self, op: &TokenKind, rhs: &Self) -> Result<Self, String> {
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
                (_, _) => Err("Operands must be two numbers.".to_string()),
            },
            TokenKind::Star => match (self, rhs) {
                (Number(n1), Number(n2)) => Ok(Number(n1 * n2)),
                (_, _) => Err("Operands must be two numbers.".to_string()),
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

#[derive(Clone)]
struct Callable {
    arity: usize,
    repr: String,
    func: CallableKind,
}

#[derive(Clone)]
enum CallableKind {
    BuiltIn(Rc<dyn Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, String>>),
    User(UserFunction),
}

impl Callable {
    fn new_user_function(arity: usize, repr: String, func: UserFunction) -> Self {
        Self {
            arity,
            repr,
            func: CallableKind::User(func),
        }
    }

    fn new_built_in_function<F>(arity: usize, repr: String, func: F) -> Self
    where
        F: Fn(&mut Interpreter, &Vec<Value>) -> Result<Value, String> + 'static,
    {
        Self {
            arity,
            repr,
            func: CallableKind::BuiltIn(Rc::new(func)),
        }
    }

    fn call(&self, interp: &mut Interpreter, arguments: &Vec<Value>) -> Result<Value, String> {
        match &self.func {
            CallableKind::BuiltIn(f) => f(interp, arguments),
            CallableKind::User(f) => f.call(interp, arguments),
        }
    }

    fn arity(&self) -> usize {
        self.arity
    }

    fn to_string(&self) -> String {
        self.repr.clone()
    }
}

impl PartialEq for Callable {
    fn eq(&self, other: &Self) -> bool {
        match (&self.func, &other.func) {
            (CallableKind::User(a), CallableKind::User(b)) => {
                // compare raw pointers to FunctionDecl
                a.decl == b.decl
            }
            (CallableKind::BuiltIn(a), CallableKind::BuiltIn(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl std::fmt::Debug for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.repr)
    }
}

type Shared<T> = Rc<RefCell<T>>;

#[derive(Clone)]
struct Environment {
    enclosing: Option<Shared<Environment>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new_shared() -> Shared<Self> {
        Rc::new(RefCell::new(Environment {
            enclosing: None,
            values: HashMap::new(),
        }))
    }

    fn new_shared_with_enclosing(enclosing: Shared<Environment>) -> Shared<Self> {
        Rc::new(RefCell::new(Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }))
    }

    fn get_from_ancestor(&self, distance: usize, name: &Token) -> Result<Value, String> {
        if distance == 0 {
            self.get(&name)
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get_from_ancestor(distance - 1, name),
                None => Err("Internal error: ancestor not found.".to_string()),
            }
        }
    }

    fn set_in_ancestor(
        &mut self,
        distance: usize,
        name: &Token,
        value: &Value,
    ) -> Result<(), String> {
        if distance == 0 {
            self.set(&name, &value)
        } else {
            match &self.enclosing {
                Some(enclosing) => {
                    enclosing
                        .borrow_mut()
                        .set_in_ancestor(distance - 1, name, value)
                }
                None => Err("Internal error: ancestor not found.".to_string()),
            }
        }
    }

    fn get(&self, name: &Token) -> Result<Value, String> {
        if let Some(value) = self.values.get(&name.lexeme) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(&name)
        } else {
            Err(format!("Undefined variable '{}'.", &name.lexeme))
        }
    }

    fn set(&mut self, name: &Token, value: &Value) -> Result<(), String> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        } else if let Some(enclosing) = &mut self.enclosing {
            enclosing.borrow_mut().set(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", &name.lexeme))
        }
    }

    fn define(&mut self, name: &Token, value: &Value) -> Result<(), String> {
        if self.values.contains_key(&name.lexeme) {
            Err(format!("Variable '{}' is already defined.", &name.lexeme))
        } else {
            self.values.insert(name.lexeme.clone(), value.clone());
            Ok(())
        }
    }
}

impl std::fmt::Debug for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(enclosing) = &self.enclosing {
            write!(f, "{:?}", enclosing.borrow())?;
        }

        for (name, value) in &self.values {
            writeln!(f, "{} : {}", name, value)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct UserFunction {
    decl: *const FunctionDeclaration,
    closure: Shared<Environment>,
}

impl UserFunction {
    fn new(decl: &FunctionDeclaration, closure: Shared<Environment>) -> Self {
        UserFunction { decl, closure }
    }

    fn call(&self, interpreter: &mut Interpreter, args: &Vec<Value>) -> Result<Value, String> {
        // create a new environment for the function call
        let env = Environment::new_shared_with_enclosing(self.closure.clone());

        // SAFETY: we know the FunctionDeclaration outlives the Interpreter
        let decl = unsafe { &*self.decl };

        // bind arguments to parameters
        for (param, arg) in decl.parameters.iter().zip(args.iter()) {
            env.borrow_mut().define(param, arg)?;
        }

        interpreter.with_environment(env, |interpreter| {
            match interpreter.interpret_block_statement(&decl.body) {
                Ok(ControlFlow::Break(return_value)) => Ok(return_value),
                Ok(ControlFlow::Continue(())) => {
                    Err("UserFunction body completed without a return value.".to_string())
                }
                Err(e) => Err(e),
            }
        })
    }
}

pub struct Interpreter {
    current_environment: Shared<Environment>,
    name_resolver: NameResolver,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            current_environment: Environment::new_shared(),
            name_resolver: NameResolver::new(),
        }
    }

    fn push_environment(&mut self) {
        self.current_environment =
            Environment::new_shared_with_enclosing(self.current_environment.clone());
    }

    fn pop_environment(&mut self) {
        let enclosing = {
            let current = self.current_environment.borrow();
            current.enclosing.clone()
        };
        self.current_environment = enclosing.expect("Can't pop global environment");
    }

    fn with_environment<T>(
        &mut self,
        new_env: Shared<Environment>,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let mut temp_env = new_env;
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        let result = f(self);
        std::mem::swap(&mut self.current_environment, &mut temp_env);
        result
    }

    fn interpret_literal(&self, lit: &Literal) -> Result<Value, String> {
        Ok(match lit {
            Literal::Bool(b) => Value::Bool(*b),
            Literal::Nil => Value::Nil,
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn interpret_grouping_expression(
        &mut self,
        expr: &GroupingExpression,
    ) -> Result<Value, String> {
        self.interpret_expression(&*expr.expr)
    }

    fn interpret_logical_expression(&mut self, expr: &LogicalExpression) -> Result<Value, String> {
        let left = self.interpret_expression(&*expr.left_expr)?;
        match expr.op.kind {
            TokenKind::Or => {
                if left.as_bool() {
                    return Ok(left);
                }
            }
            TokenKind::And => {
                if !left.as_bool() {
                    return Ok(left);
                }
            }
            _ => {
                return Err(format!(
                    "Unexpected '{}' in logical expression",
                    expr.op.lexeme
                ))
            }
        };
        return self.interpret_expression(&*expr.right_expr);
    }

    fn interpret_binary_expression(&mut self, expr: &BinaryExpression) -> Result<Value, String> {
        let lhs = self.interpret_expression(&*expr.left_expr)?;
        let rhs = self.interpret_expression(&*expr.right_expr)?;
        lhs.evaluate_binary_operation(&expr.op.kind, &rhs)
    }

    fn interpret_call_expression(&mut self, call: &CallExpression) -> Result<Value, String> {
        let value = self.interpret_expression(&*call.callee)?;

        let callee = match &value {
            Value::Callable(callee) => callee,
            _ => return Err("Can only call functions and classes.".to_string()),
        };

        if call.arguments.len() != callee.arity() {
            return Err(format!(
                "Expected {} arguments but got {}.",
                callee.arity(),
                call.arguments.len()
            ));
        }

        let mut arguments = Vec::new();
        for arg in &call.arguments {
            arguments.push(self.interpret_expression(&arg)?);
        }

        callee.call(self, &arguments)
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
        let ancestor = self.name_resolver.lookup(&var)?;
        self.current_environment
            .borrow()
            .get_from_ancestor(ancestor, &var.name)
    }

    fn interpret_assignment_expression(
        &mut self,
        expr: &AssignmentExpression,
    ) -> Result<Value, String> {
        let value = self.interpret_expression(&*expr.expr)?;
        let ancestor = self.name_resolver.lookup(&expr.var)?;
        self.current_environment
            .borrow_mut()
            .set_in_ancestor(ancestor, &expr.var.name, &value)?;
        Ok(value)
    }

    fn interpret_expression(&mut self, expr: &Expression) -> Result<Value, String> {
        match expr {
            Expression::Assignment(expr) => self.interpret_assignment_expression(expr),
            Expression::Binary(expr) => self.interpret_binary_expression(expr),
            Expression::Call(expr) => self.interpret_call_expression(expr),
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

    fn interpret_block_statement(
        &mut self,
        block: &BlockStatement,
    ) -> Result<ControlFlow<Value>, String> {
        self.push_environment();

        let mut result = Ok(ControlFlow::Continue(()));

        for stmt in &block.statements {
            match self.interpret_statement(stmt) {
                Ok(ControlFlow::Continue(())) => continue,
                Ok(ControlFlow::Break(value)) => {
                    result = Ok(ControlFlow::Break(value));
                    break;
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
        }

        self.pop_environment();

        result
    }

    fn interpret_expression_statement(&mut self, stmt: &ExpressionStatement) -> Result<(), String> {
        self.interpret_expression(&stmt.expr)?;
        Ok(())
    }

    fn interpret_function_declaration(&mut self, decl: &FunctionDeclaration) -> Result<(), String> {
        let repr = format!("<fn {}>", decl.name.lexeme);
        let func = UserFunction::new(decl, self.current_environment.clone());
        let callable = Value::Callable(Callable::new_user_function(
            decl.parameters.len(),
            repr,
            func,
        ));
        self.current_environment
            .borrow_mut()
            .define(&decl.name, &callable)
    }

    fn interpret_if_statement(&mut self, stmt: &IfStatement) -> Result<(), String> {
        let condition = self.interpret_expression(&stmt.condition)?;
        if condition.as_bool() {
            self.interpret_statement(&*stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.interpret_statement(&else_branch)?;
        }
        Ok(())
    }

    fn interpret_print_statement(&mut self, stmt: &PrintStatement) -> Result<(), String> {
        let val = self.interpret_expression(&stmt.expr)?;
        println!("{}", val);
        Ok(())
    }

    fn interpret_return_statement(
        &mut self,
        stmt: &ReturnStatement,
    ) -> Result<ControlFlow<Value>, String> {
        let value = match &stmt.expr {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        Ok(ControlFlow::Break(value))
    }

    fn interpret_variable_declaration(&mut self, decl: &VariableDeclaration) -> Result<(), String> {
        let value = match &decl.initializer {
            Some(expr) => self.interpret_expression(expr)?,
            None => Value::Nil,
        };
        self.current_environment
            .borrow_mut()
            .define(&decl.name, &value)
    }

    fn interpret_statement(&mut self, stmt: &Statement) -> Result<ControlFlow<Value>, String> {
        match stmt {
            Statement::Block(block) => self.interpret_block_statement(block),
            Statement::Return(ret) => self.interpret_return_statement(ret),

            // map the results of other statements to ControlFlow::Continue
            _ => match stmt {
                Statement::Block(_) => Err("Impossible statement".to_string()),
                Statement::Assert(assert) => self.interpret_assert_statement(assert),
                Statement::Expr(expr) => self.interpret_expression_statement(expr),
                Statement::FunDecl(fun) => self.interpret_function_declaration(fun),
                Statement::If(i) => self.interpret_if_statement(i),
                Statement::Print(print) => self.interpret_print_statement(print),
                Statement::Return(_) => Err("Impossible statement".to_string()),
                Statement::VarDecl(var) => self.interpret_variable_declaration(var),
            }
            .map(|_| ControlFlow::Continue(())),
        }
    }

    pub fn interpret_program(&mut self, prog: &Program) -> Result<(), String> {
        // first do name resolution
        self.name_resolver.resolve_program(&prog)?;

        for stmt in &prog.statements {
            self.interpret_statement(stmt)?;
        }
        Ok(())
    }
}
