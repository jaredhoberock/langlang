#pragma once

#include "syntax.hpp"
#include "token_stream.hpp"
#include <algorithm>
#include <array>
#include <concepts>
#include <fmt/core.h>
#include <optional>
#include <vector>

class parser
{
  public:
    parser(token_stream stream)
      : stream_(stream),
        current_(stream_.begin())
    {}

    program parse()
    {
      return expect_program();
    }

  private:
    token peek() const
    {
      return *current_;
    }

    token advance()
    {
      token result = *current_;

      if(current_ != stream_.end())
      {
        ++current_;
      }

      return result;
    }

    std::string error_message(const char* message)
    {
      std::string result;

      switch(peek().which_kind())
      {
        case token::eof:
        {
          result = fmt::format("{} at end: {}", peek().location(), message);
          break;
        }

        default:
        {
          result = fmt::format("{} at '{}': {}", peek().location(), peek().lexeme(), message);
          break;
        }
      }

      return result;
    }

    std::string error_message(token tok, const char* message)
    {
      std::string result;

      switch(tok.which_kind())
      {
        case token::eof:
        {
          result = fmt::format("{} at end: {}", tok.location(), message);
          break;
        }

        default:
        {
          result = fmt::format("{} at '{}': {}", tok.location(), tok.lexeme(), message);
          break;
        }
      }

      return result;
    }

    void throw_error(token tok, const char* message)
    {
      throw std::runtime_error(error_message(tok, message));
    }

    void throw_error(const char* message)
    {
      throw_error(peek(), message);
    }

    token expect_token(token::kind k, const char* message)
    {
      if(peek().which_kind() != k)
      {
        throw_error(fmt::format("Expected '{}' {}", token::to_string(k), message).c_str());
      }

      return advance();
    }

    // primary := number | string | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
    expression expect_primary()
    {
      if(std::optional n = match(token::number))
      {
        return literal{n->number_literal()};
      }
      else if(std::optional s = match(token::string))
      {
        return literal{s->string_literal()};
      }
      else if(match(token::true_))
      {
        return literal{true};
      }
      else if(match(token::false_))
      {
        return literal{false};
      }
      else if(match(token::nil))
      {
        return literal{nullptr};
      }
      else if(match(token::left_paren))
      {
        expression expr = expect_expression();

        expect_token(token::right_paren, "after expression.");

        return grouping_expression{expr};
      }
      else if(std::optional name = match(token::identifier))
      {
        return variable{*name};
      }

      throw_error("Expected primary expression.");

      // XXX this sucks
      return literal{0.0};
    }

    // arguments := expression ( "," expression )*
    std::vector<expression> expect_arguments()
    {
      std::vector<expression> result;

      if(peek().which_kind() != token::right_paren)
      {
        if(result.size() == 255)
        {
          throw_error("Can't have more than 255 arguments.");
        }

        do
        {
          result.push_back(expect_expression());
        }
        while(match(token::comma));
      }

      return result;
    }

    // call := primary ( "(" arguments? ")" )*
    expression expect_call()
    {
      expression result = expect_primary();

      if(match(token::left_paren))
      {
        std::vector<expression> arguments = expect_arguments();

        token paren = expect_token(token::right_paren, "after arguments.");

        result = call_expression{std::move(result), arguments, paren};
      }

      return result;
    }

    // unary := ( "!" | "-" ) unary | call
    expression expect_unary()
    {
      if(std::optional op = match_any(token::bang, token::minus))
      {
        return unary_expression{*op, expect_unary()};
      }

      return expect_call();
    }

    // factor := unary ( ( "/" | "*" ) ) unary )*
    expression expect_factor()
    {
      expression result = expect_unary();

      while(std::optional op = match_any(token::slash, token::star))
      {
        expression right = expect_unary();

        result = binary_expression{result, *op, right};
      }

      return result;
    }

    // term := factor ( ( "-" | "+" ) factor )*
    expression expect_term()
    {
      expression result = expect_factor();

      while(std::optional op = match_any(token::minus, token::plus))
      {
        expression right = expect_factor();

        result = binary_expression{result, *op, right};
      }

      return result;
    }

    // comparison := term ( ( ">" | ">=" | "<=" ) term )*
    expression expect_comparison()
    {
      expression result = expect_term(); 

      while(std::optional op = match_any(token::greater, token::greater_equal, token::less, token::less_equal))
      {
        expression right = expect_term();

        result = binary_expression{result, *op, right};
      }

      return result;
    }

    // equality := comparison ( ( "!=" | "==" ) comparison )*
    expression expect_equality()
    {
      expression result = expect_comparison();

      while(std::optional op = match_any(token::bang_equal, token::equal_equal))
      {
        expression right = expect_comparison();

        result = binary_expression{result, *op, right};
      }

      return result;
    }

    // logical_and := equality ( "and" equality )*
    expression expect_logical_and()
    {
      expression result = expect_equality();

      if(std::optional and_ = match(token::and_))
      {
        result = logical_expression{result, *and_, expect_equality()};
      }

      return result;
    }

    // logical_or := logical_and ( "or" logical_and )*
    expression expect_logical_or()
    {
      expression result = expect_logical_and();

      if(std::optional or_ = match(token::or_))
      {
        result = logical_expression{result, *or_, expect_logical_and()};
      }

      return result;
    }

    // assignment := IDENTIFIER "=" assignment | logical_or
    expression expect_assignment()
    {
      expression result = expect_logical_or();

      if(std::optional eq = match(token::equal))
      {
        if(not std::holds_alternative<variable>(result))
        {
          throw_error(*eq, "Invalid assignment target.");
        }

        expression rhs = expect_assignment();

        result = assignment_expression{get<variable>(result), rhs};
      }

      return result;
    }

    // expression := assignment
    expression expect_expression()
    {
      return expect_assignment();
    }

    // expression_statement := expression ";"
    expression_statement expect_expression_statement()
    {
      expression expr = expect_expression();

      token consumed_tok = expect_token(token::semicolon, "after expression.");

      return {expr};
    }

    // print_statement := "print" expression ";"
    print_statement expect_print_statement()
    {
      expect_token(token::print, "before expression.");

      expression expr = expect_expression();

      expect_token(token::semicolon, "after print expression.");

      return {expr};
    }

    // if_statement := "if" "(" expression ")" statement ( "else" statement )?
    if_statement expect_if_statement()
    {
      expect_token(token::if_, "before condition.");

      expect_token(token::left_paren, "after 'if'.");

      expression expr = expect_expression();

      expect_token(token::right_paren, "after if condition.");

      statement then_branch = expect_statement();

      std::optional<statement> else_branch;

      if(match(token::else_))
      {
        else_branch = expect_statement();
      }

      return {expr, then_branch, else_branch};
    }

    // return_statement := "return" expression? ";"
    return_statement expect_return_statement()
    {
      token keyword = expect_token(token::return_, "before expression.");

      std::optional<expression> expr;

      if(peek().which_kind() != token::semicolon)
      {
        expr = expect_expression();
      }

      expect_token(token::semicolon, "after return value.");

      return {keyword, expr};
    }

    // while_statement := "while" "(" expression ")" statement
    while_statement expect_while_statement()
    {
      expect_token(token::while_, "before condition.");

      expect_token(token::left_paren, "after 'while'.");

      expression expr = expect_expression();

      expect_token(token::right_paren, "after while condition.");

      statement body = expect_statement();

      return {expr, body};
    }

    // for_statement := "for" "(" ( variable_declaration | expression_statement ) expression? ";" expression? ")" statement
    for_statement expect_for_statement()
    {
      expect_token(token::for_, "before loop.");

      expect_token(token::left_paren, "after 'for'.");

      std::optional<statement> initializer;
      if(not match(token::semicolon))
      {
        if(peek().which_kind() == token::var)
        {
          initializer = expect_variable_declaration();
        }
        else
        {
          initializer = expect_expression_statement();
        }
      }

      std::optional<expression> condition;
      if(not match(token::semicolon))
      {
        condition = expect_expression();
      }

      expect_token(token::semicolon, "after for loop condition.");

      std::optional<expression> increment;
      if(not match(token::right_paren))
      {
        increment = expect_expression();
      }

      expect_token(token::right_paren, "after for loop increment.");

      statement body = expect_statement();

      return {initializer, condition, increment, body};
    }

    // block_statement := "{" declaration* "}"
    block_statement expect_block_statement()
    {
      expect_token(token::left_brace, "before block.");

      std::vector<statement> stmts;

      while(peek().which_kind() != token::right_brace)
      {
        stmts.push_back(expect_declaration());
      }

      expect_token(token::right_brace, "after block.");

      return {stmts};
    }

    // statement := expression_statement | print_statement | if_statement | return_statement | while_statement | for_statement | "{" declaration* "}"
    statement expect_statement()
    {
      if(peek().which_kind() == token::print)
      {
        return expect_print_statement();
      }
      else if(peek().which_kind() == token::if_)
      {
        return expect_if_statement();
      }
      else if(peek().which_kind() == token::return_)
      {
        return expect_return_statement();
      }
      else if(peek().which_kind() == token::while_)
      {
        return expect_while_statement();
      }
      else if(peek().which_kind() == token::for_)
      {
        return expect_for_statement();
      }
      else if(peek().which_kind() == token::left_brace)
      {
        return expect_block_statement();
      }

      return expect_expression_statement();
    }

    // variable_declaration := "var" IDENTIFIER ( "=" expression )? ";"
    variable_declaration expect_variable_declaration()
    {
      expect_token(token::var, "before variable name.");

      token name = expect_token(token::identifier, ".");

      std::optional<expression> initializer;

      if(match(token::equal))
      {
        initializer = expect_expression();
      }

      expect_token(token::semicolon, "after variable declaration.");

      return {name, initializer};
    }

    // parameters := IDENTIFIER ( "," IDENTIFIER )*
    std::vector<token> expect_parameters()
    {
      std::vector<token> result;

      if(peek().which_kind() != token::right_paren)
      {
        if(result.size() == 255)
        {
          throw_error("Can't have more than 255 parameters.");
        }

        do
        {
          result.push_back(expect_token(token::identifier, "in function parameter list."));
        }
        while(match(token::comma));
      }

      return result;
    }

    // function_declaration := "fun" IDENTIFIER "(" parameters? ")" block_statement
    function_declaration expect_function_declaration()
    {
      expect_token(token::fun, "before function name.");

      token name = expect_token(token::identifier, ".");

      expect_token(token::left_paren, "before function parameters.");

      std::vector<token> parameters = expect_parameters();

      expect_token(token::right_paren, "after function parameters.");

      block_statement body = expect_block_statement();

      return {name, parameters, body};
    }

    // declaration := function_declaration | variable_declaration | statement
    statement expect_declaration()
    {
      if(peek().which_kind() == token::fun)
      {
        return expect_function_declaration();
      }
      else if(peek().which_kind() == token::var)
      {
        return expect_variable_declaration();
      }

      return expect_statement();
    }

    // program := declaration* EOF
    program expect_program()
    {
      std::vector<statement> statements;

      while(peek().which_kind() != token::eof)
      {
        statements.push_back(expect_declaration());
      }

      // consume eof
      expect_token(token::eof, ".");

      return {statements};
    }

    std::optional<token> match(token::kind kind)
    {
      std::optional<token> result;

      if(peek().which_kind() == kind)
      {
        result = advance();
      }

      return result;
    }

    template<std::same_as<token::kind>... TokenKinds>
    std::optional<token> match_any(token::kind arg, TokenKinds... args)
    {
      std::optional<token> result;

      std::array<token::kind, 1 + sizeof...(args)> kinds{arg, args...};

      bool found = std::find(kinds.begin(), kinds.end(), peek().which_kind()) != kinds.end();

      if(found)
      {
        result = advance();
      }

      return result;
    }

    token_stream stream_;
    token_stream::iterator current_;
};

