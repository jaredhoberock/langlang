#pragma once

#include "recursive_variant.hpp"
#include "token_range.hpp"
#include <fmt/core.h>
#include <vector>

using literal = std::variant<double, std::string, bool, std::nullptr_t>;

struct variable
{
  token name;
};

using expression = recursive_variant<
  literal, struct unary_expression, struct binary_expression,
  struct grouping_expression, variable, struct assignment_expression,
  struct logical_expression,
  struct call_expression
>;

struct unary_expression
{
  token op;
  expression right_expr;
};

struct binary_expression
{
  expression left_expr;
  token op;
  expression right_expr;
};

struct grouping_expression
{
  expression expr;
};

struct assignment_expression
{
  variable var;
  expression expr;
};

struct logical_expression
{
  expression left_expr;
  token op;
  expression right_expr;
};

struct call_expression
{
  expression callee;
  std::vector<expression> arguments;
  token closing_paren;
};

struct assert_statement
{
  expression expr;
};

struct return_statement
{
  token keyword;
  std::optional<expression> expr;
};

struct print_statement
{
  expression expr;
};

struct variable_declaration
{
  token name;
  std::optional<expression> initializer;
};

using statement = recursive_variant<
  assert_statement, return_statement, print_statement, variable_declaration, 
  struct expression_statement, struct block_statement,
  struct function_declaration, struct if_statement,
  struct while_statement, struct for_statement
>;

struct expression_statement
{
  expression expr;
};

struct block_statement
{
  std::vector<statement> statements;
};

struct function_declaration
{
  token name;
  std::vector<token> parameters;
  block_statement body;
};

struct if_statement
{
  expression condition;
  statement then_branch;
  std::optional<statement> else_branch;
};

struct while_statement
{
  expression condition;
  statement body;
};

struct for_statement
{
  std::optional<statement>  initializer;
  std::optional<expression> condition;
  std::optional<expression>  increment;
  statement body;
};

struct program
{
  std::vector<statement> statements;
};

struct syntax_printer
{
  [[no_unique_address]] syntax_printer& self = *this;

  std::string operator()(const double& d) const
  {
    return fmt::format("{}", d);
  }

  std::string operator()(const std::string& s) const
  {
    return fmt::format("\"{}\"", s);
  }

  std::string operator()(bool b) const
  {
    return b ? "true" : "false";
  }

  std::string operator()(std::nullptr_t) const
  {
    return "nil";
  }

  std::string operator()(const literal& l) const
  {
    return visit(self, l);
  }

  std::string operator()(const variable& v) const
  {
    return v.name.lexeme();
  }

  std::string operator()(const unary_expression& expr) const
  {
    return fmt::format("({} {})", expr.op.lexeme(), self(expr.right_expr));
  }

  std::string operator()(const binary_expression& expr) const
  {
    return fmt::format("({} {} {})", expr.op.lexeme(), self(expr.left_expr), self(expr.right_expr));
  }

  std::string operator()(const grouping_expression& expr) const
  {
    return fmt::format("(group {})", self(expr.expr));
  }

  std::string operator()(const assignment_expression& expr) const
  {
    return fmt::format("(= {} {})", self(expr.var), self(expr.expr));
  }

  std::string operator()(const logical_expression& expr) const
  {
    return fmt::format("({} {} {})", expr.op.lexeme(), self(expr.left_expr), self(expr.right_expr));
  }

  std::string operator()(const expression& expr) const
  {
    return visit(self, expr);
  }

  std::string operator()(const expression_statement& stmt) const
  {
    return fmt::format("{};", self(stmt.expr));
  }

  std::string operator()(const print_statement& stmt) const
  {
    return fmt::format("print {};", self(stmt.expr));
  }

  std::string operator()(const statement& stmt) const
  {
    return visit(self, stmt);
  }

  std::string operator()(const variable_declaration& decl) const
  {
    std::string result;

    if(decl.initializer)
    {
      result = fmt::format("var {} = {}", decl.name.lexeme(), visit(self,*decl.initializer));
    }
    else
    {
      result = fmt::format("var {}", decl.name.lexeme());
    }

    return result;
  }
};

