#pragma once

#include "syntax.hpp"
#include <chrono>
#include <concepts>
#include <fmt/core.h>
#include <functional>
#include <map>
#include <memory>

// XXX we could say that a callable is a builtin_function or a user_function

using value = recursive_variant<double, std::string, bool, std::nullptr_t, struct callable>;


struct callable
{
  using function_type = std::function<value(const std::vector<value>&)>;

  callable(std::size_t arity, std::string&& repr, function_type&& func)
    : arity_{arity}, repr_{repr}, func_{std::move(func)}
  {}

  value operator()(const std::vector<value>& arguments)
  {
    return func_(arguments);
  }

  std::size_t arity() const
  {
    return arity_;
  }

  bool operator==(const callable& rhs) const
  {
    return this == &rhs;
  }

  bool operator!=(const callable& rhs) const
  {
    return !operator==(rhs);
  }

  std::string to_string() const
  {
    return repr_;
  }

  private:
    std::size_t arity_;
    std::string repr_;
    function_type func_;
};


value to_value(const literal& lit)
{
  return visit([](const auto& arg) -> value
  {
    return arg;
  }, lit);
}


bool to_bool(const value& val)
{
  bool result = true;

  if(std::holds_alternative<std::nullptr_t>(val))
  {
    result = false;
  }
  else if(std::holds_alternative<bool>(val))
  {
    result = get<bool>(val);
  }

  return result;
}

std::string to_string(const value& val)
{
  std::string result;

  if(std::holds_alternative<double>(val))
  {
    result = fmt::format("{}", get<double>(val));
  }
  else if(std::holds_alternative<std::string>(val))
  {
    result = get<std::string>(val);
  }
  else if(std::holds_alternative<bool>(val))
  {
    result = get<bool>(val) ? "true" : "false";
  }
  else if(std::holds_alternative<std::nullptr_t>(val))
  {
    result = "nil";
  }
  else if(std::holds_alternative<callable>(val))
  {
    result = get<callable>(val).to_string();
  }

  return result;
}

std::string error_message(token t, const char* message)
{
  return fmt::format("{} at '{}': {}", t.location(), t.lexeme(), message);
}


statement desugar_for_statement(for_statement&& stmt)
{
  std::optional initializer = std::move(stmt.initializer);
  std::optional condition = std::move(stmt.condition);
  std::optional increment = std::move(stmt.increment);

  statement result = std::move(stmt.body);

  // if the loop increment exists, append it to the loop body
  if(increment)
  {
    std::vector<statement> statements = {std::move(result), expression_statement{std::move(*increment)}};
    result = block_statement{std::move(statements)};
  }
  
  // if the loop condition does not exist, create a constant condition
  if(not condition)
  {
    condition = literal{true};
  }
  
  // create a while loop
  result = while_statement{*condition, std::move(result)};
  
  // if the loop initializer exists, introduce it into a new block containing it and the loop
  if(initializer)
  {
    std::vector<statement> statements = {std::move(*initializer), std::move(result)};
    result = block_statement{std::move(statements)};
  }
  
  return result;
}


class environment : public std::enable_shared_from_this<environment>
{
  public:
    environment(environment& enclosing)
      : enclosing_{enclosing.share()}, values_{}
    {}

    using value_type = std::map<std::string,value>::value_type;

    environment()
      : enclosing_{nullptr}, values_{}
    {}

    environment(std::map<std::string,value> initial_values)
      : enclosing_{nullptr}, values_{std::move(initial_values)}
    {}

    std::shared_ptr<environment> share()
    {
      return shared_from_this();
    }

    void define(const std::string& name, const value& val)
    {
      values_[name] = val;
    }

    value& lookup(const token& name)
    {
      auto found = values_.find(name.lexeme());

      if(found == values_.end() and enclosing_ == nullptr)
      {
        std::string msg = fmt::format("Undefined variable '{}'.", name.lexeme());
        throw std::runtime_error(error_message(name, msg.c_str()));
      }

      return found != values_.end() ? found->second : enclosing_->lookup(name);
    }

    void dump() const
    {
      if(enclosing_)
      {
        enclosing_->dump();
      }

      for(const auto& v : values_)
      {
        std::cout << v.first << " : " << to_string(v.second) << std::endl;
      }
    }

  private:
    std::shared_ptr<environment> enclosing_;
    std::map<std::string, value> values_;
};


struct user_function
{
  function_declaration decl;
  std::shared_ptr<environment> closure;

  value operator()(const std::vector<value>& args);
};


struct interpreter
{
  [[no_unique_address]] interpreter& self = *this;
  std::shared_ptr<environment> global_env;

  interpreter()
    : global_env{std::make_shared<environment>(initial_global_values())}
  {}

  static void check_number_operand(token op, const value& val)
  {
    if(not std::holds_alternative<double>(val))
    {
      throw std::runtime_error(error_message(op, "Operand must be a number"));
    }

    return;
  }

  static void check_number_operands(token op, const value& l, const value& r)
  {
    if(not std::holds_alternative<double>(l) or not std::holds_alternative<double>(r))
    {
      throw std::runtime_error(error_message(op, "Operands must be numbers"));
    }

    return;
  }

  value operator()(environment&, const literal& l)
  {
    return to_value(l);
  }

  value operator()(environment& env, const grouping_expression& expr)
  {
    return self(env, expr.expr);
  }

  value operator()(environment& env, const unary_expression& expr)
  {
    value result = self(env, expr.right_expr);

    switch(expr.op.which_kind())
    {
      case token::bang:
      {
        result = not to_bool(result);
        break;
      }

      case token::minus:
      {
        result = -get<double>(result);
        break;
      }

      default:
      {
        throw std::runtime_error(error_message(expr.op, "Bad operator in unary_expression"));
        break;
      }
    }

    return result;
  }

  value operator()(environment& env, const binary_expression& expr)
  {
    value result = nullptr;

    value left = self(env, expr.left_expr);
    value right = self(env, expr.right_expr);

    switch(expr.op.which_kind())
    {
      case token::bang_equal:
      {
        result = left != right;
        break;
      }

      case token::equal_equal:
      {
        result = left == right;
        break;
      }

      case token::greater:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) > get<double>(right);
        break;
      }

      case token::greater_equal:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) >= get<double>(right);
        break;
      }

      case token::less:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) < get<double>(right);
        break;
      }

      case token::less_equal:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) <= get<double>(right);
        break;
      }

      case token::minus:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) - get<double>(right);
        break;
      }

      case token::plus:
      {
        if(std::holds_alternative<double>(left) and std::holds_alternative<double>(right))
        {
          result = get<double>(left) + get<double>(right);
        }
        else if(std::holds_alternative<std::string>(left) and std::holds_alternative<std::string>(right))
        {
          result = get<std::string>(left) + get<std::string>(right);
        }
        else
        {
          throw std::runtime_error(error_message(expr.op, "Operands must be two numbers or two strings."));
        }

        break;
      }

      case token::slash:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) / get<double>(right);
        break;
      }
      
      case token::star:
      {
        check_number_operands(expr.op, left, right);
        result = get<double>(left) * get<double>(right);
        break;
      }

      default:
      {
        throw std::runtime_error(error_message(expr.op, "Bad operator in binary_expression"));
        break;
      }
    }

    return result;
  }

  value operator()(environment& env, const variable& var)
  {
    return env.lookup(var.name);
  }

  value operator()(environment& env, const assignment_expression& expr)
  {
    return env.lookup(expr.var.name) = self(env, expr.expr);
  }

  value operator()(environment& env, const logical_expression& expr)
  {
    value left = self(env, expr.left_expr);

    if(expr.op.which_kind() == token::or_)
    {
      if(to_bool(left)) return left;
    }
    else
    {
      if(!to_bool(left))
      {
        return left;
      }
    }

    return self(env, expr.right_expr);
  }

  value operator()(environment& env, const call_expression& expr)
  {
    value callee = self(env, expr.callee);

    if(not std::holds_alternative<callable>(callee))
    {
      throw std::runtime_error(error_message(expr.closing_paren, "Can only call functions and classes."));
    }

    callable& function = std::get<callable>(callee);

    if(expr.arguments.size() != function.arity())
    {
      std::string msg = fmt::format("Expected {} arguments but got {}.", function.arity(), expr.arguments.size());
      throw std::runtime_error(error_message(expr.closing_paren, msg.c_str()));
    }

    std::vector<value> arguments;
    for(const expression& arg : expr.arguments)
    {
      arguments.push_back(self(env, arg));
    }

    return function(arguments);
  }

  value operator()(environment& env, const expression& expr)
  {
    // we have to do create this lambda to be able to pass the env as the first parameter to self
    return visit([&](const auto& e)
    {
      return self(env, e);
    }, expr);
  }

  void operator()(environment& env, const expression_statement& stmt)
  {
    // we have to do create this lambda to be able to pass the env as the first parameter to self
    visit([&](const auto& expr) -> void
    {
      self(env, expr);
    }, stmt.expr);
  }

  void operator()(environment& env, const print_statement& stmt)
  {
    value val = self(env, stmt.expr);
    std::cout << to_string(val) << std::endl;
  }

  void operator()(environment& env, const return_statement& stmt)
  {
    value result = stmt.expr ? self(env, *stmt.expr) : nullptr;
    throw result;
  }

  void operator()(environment& env, const statement& stmt)
  {
    // we have to do create this lambda to be able to pass the env as the first parameter to self
    visit([&](const auto& s)
    {
      self(env, s);
    }, stmt);
  }

  void operator()(environment& env, const variable_declaration& decl)
  {
    value val = decl.initializer ? self(env, *decl.initializer) : nullptr;

    env.define(decl.name.lexeme(), val);
  }

  void operator()(environment& env, const function_declaration& decl)
  {
    std::string repr = fmt::format("<fn {}>", decl.name.lexeme());

    value func = callable{
      decl.parameters.size(),
      std::move(repr),
      user_function{decl, env.share()}
    };

    env.define(decl.name.lexeme(), func);
  }

  void operator()(environment& env, const if_statement& stmt)
  {
    value condition = self(env, stmt.condition);
    if(to_bool(condition))
    {
      self(env, stmt.then_branch);
    }
    else if(stmt.else_branch)
    {
      self(env, *stmt.else_branch);
    }
  }

  void operator()(environment& env, const while_statement& stmt)
  {
    while(to_bool(self(env, stmt.condition)))
    {
      self(env, stmt.body);
    }
  }

  void operator()(environment& env, const for_statement& stmt)
  {
    // create a nested environment
    std::shared_ptr<environment> nested_env = std::make_shared<environment>(env);

    if(stmt.initializer)
    {
      self(*nested_env, *stmt.initializer);
    }

    while(not stmt.condition or to_bool(self(*nested_env, *stmt.condition)))
    {
      self(*nested_env, stmt.body);

      if(stmt.increment)
      {
        self(*nested_env, *stmt.increment);
      }
    }
  }

  void operator()(environment& env, const block_statement& block)
  {
    // create a nested environment
    std::shared_ptr<environment> nested_env = std::make_shared<environment>(env);

    for(auto& stmt : block.statements)
    {
      self(*nested_env, stmt);
    }
  }

  void operator()(const program& program)
  {
    for(auto stmt: program.statements)
    {
      self(*global_env, stmt);
    }
  }

  static std::map<std::string,value> initial_global_values()
  {
    const auto program_started = std::chrono::system_clock::now();
    
    auto clock_function = callable{0, "<native fn>", [=](const std::vector<value>&) -> value
    {
      using namespace std::chrono;
    
      double elapsed = duration_cast<seconds>(system_clock::now() - program_started).count();
    
      return elapsed;
    }};

    return {{"clock", clock_function}};
  }
};


value user_function::operator()(const std::vector<value>& args)
{
  // create an environment for this call
  std::shared_ptr<environment> env = std::make_shared<environment>(*closure);

  // bind arguments to parameters
  for(size_t i = 0; i != decl.parameters.size(); ++i)
  {
    env->define(decl.parameters[i].lexeme(), args[i]);
  }

  value result = nullptr;

  try
  {
    // interpret the function body
    interpreter interp;
    interp(*env, decl.body);
  }
  // XXX catching by value here causes a segfault when compiled by circle
  catch(value r)
  {
    result = r;
  }

  return result;
}

