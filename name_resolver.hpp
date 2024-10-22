#pragma once

#include "syntax.hpp"
#include <cassert>
#include <fmt/format.h>
#include <variant>
#include <map>
#include <vector>

struct name_resolver
{
  // name_resolver updates this symbol_table as it resolves variable names
  // this table maps each resolved name to the scope in which its referent is defined
  std::map<const variable*, int>& symbol_table;

  name_resolver& self = *this;

  // maps a name to whether or not it has been defined
  std::vector<std::map<std::string,bool>> scopes = {};

  constexpr void resolve_variable(const variable& expr, const std::string& name)
  {
    assert(not scopes.empty());

    // look through each scope, starting at the top of
    // the stack for a declaration for name, and note
    // the location of its scope if found
    for(int i = scopes.size() - 1; i >= 0; --i)
    {
      if(scopes[i].contains(name))
      {
        // we record the distance that we need to "climb" from
        // the innermost scope to find the name's referent
        symbol_table[&expr] = scopes.size() - 1 - i;
        return;
      }
    }

    throw std::runtime_error(std::format("Name resolution error: '{}' is undefined\n", name));
  }

  void declare(const token& name)
  {
    assert(not scopes.empty());

    scopes.back()[name.lexeme()] = false;
  }

  void define(const token& name)
  {
    assert(not scopes.empty());
    assert(scopes.back().contains(name.lexeme()));

    scopes.back()[name.lexeme()] = true;
  }

  void operator()(const variable_declaration& decl)
  {
    declare(decl.name);
    if(decl.initializer)
    {
      self(*decl.initializer);
    }
    define(decl.name);
  }

  void operator()(const literal&)
  {
    return;
  }

  void operator()(const unary_expression& expr)
  {
    self(expr.right_expr);
  }

  void operator()(const binary_expression& expr)
  {
    self(expr.left_expr);
    self(expr.right_expr);
  }

  void operator()(const grouping_expression& expr)
  {
    self(expr.expr);
  }

  void operator()(const variable& var)
  {
    assert(not scopes.empty());

    const token& tok = var.name;

    if(scopes.back().contains(tok.lexeme()))
    {
      if(scopes.back()[tok.lexeme()] == false)
      {
        throw std::runtime_error(std::format("Name resolution error: {} at '{}': Can't read variable in its own initializer.", tok.location(), tok.lexeme()));
      }
    }

    resolve_variable(var, var.name.lexeme());
  }

  void operator()(const assignment_expression& assign)
  {
    self(assign.expr);
    self(assign.var);
  }

  void operator()(const logical_expression& expr)
  {
    self(expr.left_expr);
    self(expr.right_expr);
  }

  void operator()(const call_expression& expr)
  {
    self(expr.callee);
    for(const auto& arg : expr.arguments)
    {
      self(arg);
    }
  }

  void operator()(const expression& expr)
  {
    visit(self, expr);
  }

  void operator()(const assert_statement& stmt)
  {
    self(stmt.expr);
  }

  void operator()(const return_statement& stmt)
  {
    if(stmt.expr)
    {
      self(*stmt.expr);
    }
  }

  void operator()(const print_statement& stmt)
  {
    self(stmt.expr);
  }

  void operator()(const expression_statement& stmt)
  {
    self(stmt.expr);
  }

  void operator()(const block_statement& block)
  {
    scopes.emplace_back();
    for(const auto& stmt : block.statements)
    {
      self(stmt);
    }
    scopes.pop_back();
  }

  void operator()(const function_declaration& decl)
  {
    declare(decl.name);
    define(decl.name);

    scopes.emplace_back();
    for(const auto& param : decl.parameters)
    {
      declare(param);
      define(param);
    }
    self(decl.body);
    scopes.pop_back();
  }

  void operator()(const if_statement& stmt)
  {
    self(stmt.condition);
    self(stmt.then_branch);
    if(stmt.else_branch)
    {
      self(*stmt.else_branch);
    }
  }

  void operator()(const while_statement& stmt)
  {
    self(stmt.condition);
    self(stmt.body);
  }

  void operator()(const for_statement& stmt)
  {
    scopes.emplace_back();
    if(stmt.initializer)
    {
      self(*stmt.initializer);
    }

    if(stmt.condition)
    {
      self(*stmt.condition);
    }

    if(stmt.increment)
    {
      self(*stmt.increment);
    }

    self(stmt.body);
    scopes.pop_back();
  }

  void operator()(const statement& stmt)
  {
    visit(self,stmt);
  }

  void operator()(const program& prog)
  {
    assert(scopes.empty());

    scopes.emplace_back();
    for(const auto& stmt : prog.statements)
    {
      self(stmt);
    }
    scopes.pop_back();

    assert(scopes.empty());
  }
};

