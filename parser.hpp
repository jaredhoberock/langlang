#pragma once

#include "syntax.hpp"
#include "token_stream.hpp"
#include <algorithm>
#include <array>
#include <concepts>
#include <expected>
#include <fmt/core.h>
#include <optional>
#include <vector>


struct format_error_t
{
  const char* fmt;

  constexpr std::string operator()(std::string&& error) const
  {
    return std::vformat(fmt, std::make_format_args(error));
  }
};

constexpr format_error_t format_error(const char* fmt)
{
  return {fmt};
}

template<class T>
constexpr auto operator|(std::expected<T,std::string>&& result, format_error_t xfrm)
{
  return std::move(result).transform_error(xfrm);
}


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
          result = fmt::format("{} at end: {}.", peek().location(), message);
          break;
        }

        default:
        {
          result = fmt::format("{} at '{}': {}.", peek().location(), peek().lexeme(), message);
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
          result = fmt::format("{} at end: {}.", tok.location(), message);
          break;
        }

        default:
        {
          result = fmt::format("{} at '{}': {}.", tok.location(), tok.lexeme(), message);
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

    void throw_error(const std::string& message)
    {
      throw_error(peek(), message.c_str());
    }

    template<class T>
    void throw_error(const std::expected<T,std::string>& result)
    {
      throw_error(result.error());
    }

    std::expected<token,std::string> parse_token(token::kind k)
    {
      if(peek().which_kind() != k)
      {
        return std::unexpected(std::format("Expected '{}'", token::to_string(k)));
      }

      return advance();
    }

    std::expected<token,std::string> parse_token(char c)
    {
      return parse_token(token::to_kind(c));
    }

    // primary := number | string | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
    std::expected<expression,std::string> parse_primary()
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
        auto expr = parse_expression();
        if(not expr) throw_error(expr);

        auto rparen = parse_token(')') | format_error("{} after expression");
        if(not rparen) throw_error(rparen);

        return grouping_expression{*expr};
      }
      else if(std::optional name = match(token::identifier))
      {
        return variable{*name};
      }

      return std::unexpected("Expected primary expression");
    }

    // arguments := expression ( "," expression )*
    std::expected<std::vector<expression>,std::string> parse_arguments()
    {
      std::vector<expression> result;

      if(peek().which_kind() != token::right_paren)
      {
        if(result.size() == 255)
        {
          return std::unexpected("Can't have more than 255 arguments");
        }

        do
        {
          auto expr = parse_expression();
          if(not expr) return std::unexpected(expr.error());

          result.push_back(*expr);
        }
        while(match(token::comma));
      }

      return result;
    }

    // call := primary ( "(" arguments? ")" )*
    std::expected<expression,std::string> parse_call()
    {
      auto result = parse_primary();
      if(not result) return result;

      if(match(token::left_paren))
      {
        auto args = parse_arguments();
        if(not args) return std::unexpected(args.error());

        auto rparen = parse_token(')') | format_error("{} after arguments");
        if(not rparen) return std::unexpected(rparen.error());

        result = call_expression{std::move(*result), *args, *rparen};
      }

      return result;
    }

    // unary := ( "!" | "-" ) unary | call
    std::expected<expression,std::string> parse_unary()
    {
      if(std::optional op = match_any(token::bang, token::minus))
      {
        return parse_unary().transform([&](expression&& result)
        {
          return unary_expression{*op, std::move(result)};
        });
      }

      return parse_call();
    }

    // factor := unary ( ( "/" | "*" ) ) unary )*
    std::expected<expression,std::string> parse_factor()
    {
      auto result = parse_unary();
      if(not result) return result;

      while(std::optional op = match_any(token::slash, token::star))
      {
        auto right = parse_unary();
        if(not right) return right;

        result = binary_expression{*result, *op, *right};
      }

      return result;
    }

    // term := factor ( ( "-" | "+" ) factor )*
    expression expect_term()
    {
      auto result = parse_factor();
      if(not result) throw_error(result);

      while(std::optional op = match_any(token::minus, token::plus))
      {
        auto right = parse_factor();
        if(not right) throw_error(right);

        result = binary_expression{*result, *op, *right};
      }

      return *result;
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
    std::expected<expression,std::string> parse_expression()
    {
      return expect_assignment();
    }

    // expression_statement := expression ";"
    expression_statement expect_expression_statement()
    {
      auto expr = parse_expression();
      if(not expr) throw_error(expr);

      auto semi = parse_token(';') | format_error("{} after expression");
      if(not semi) throw_error(semi);

      return {*expr};
    }

    // print_statement := "print" expression ";"
    print_statement expect_print_statement()
    {
      auto print = parse_token(token::print) | format_error("{} before expression");
      if(not print) throw_error(print);

      auto expr = parse_expression();
      if(not expr) throw_error(expr);

      auto semi = parse_token(';') | format_error("{} after print expression");
      if(not semi) throw_error(semi);

      return {*expr};
    }

    // if_statement := "if" "(" expression ")" statement ( "else" statement )?
    if_statement expect_if_statement()
    {
      auto if_ = parse_token(token::if_) | format_error("{} before condition");
      if(not if_) throw_error(if_);

      auto lparen = parse_token('(') | format_error("{} after 'if'");
      if(not lparen) throw_error(lparen);

      auto expr = parse_expression();
      if(not expr) throw_error(expr);

      auto rparen = parse_token(')') | format_error("{} after if condition");
      if(not rparen) throw_error(rparen);

      statement then_branch = expect_statement();

      std::optional<statement> else_branch;

      if(match(token::else_))
      {
        else_branch = expect_statement();
      }

      return {*expr, then_branch, else_branch};
    }

    // return_statement := "return" expression? ";"
    return_statement expect_return_statement()
    {
      auto ret = parse_token(token::return_) | format_error("{} before expression");
      if(not ret) throw_error(ret);

      std::optional<expression> maybe_expr;

      if(peek().which_kind() != token::semicolon)
      {
        auto expr = parse_expression();
        if(not expr) throw_error(expr);
        maybe_expr = *expr;
      }

      auto semi = parse_token(';') | format_error("{} after return value");
      if(not semi) throw_error(semi);

      return {*ret, maybe_expr};
    }

    // while_statement := "while" "(" expression ")" statement
    while_statement expect_while_statement()
    {
      auto while_ = parse_token(token::while_) | format_error("{} before condition");
      if(not while_) throw_error(while_);

      auto lparen = parse_token('(') | format_error("{} after 'while'.");
      if(not lparen) throw_error(lparen);

      auto expr = parse_expression();
      if(not expr) throw_error(expr);

      auto rparen = parse_token(')') | format_error("{} after while condition.");
      if(not rparen) throw_error(rparen);

      statement body = expect_statement();

      return {*expr, body};
    }

    // for_statement := "for" "(" ( variable_declaration | expression_statement ) expression? ";" expression? ")" statement
    for_statement expect_for_statement()
    {
      auto for_ = parse_token(token::for_) | format_error("{} before loop.");
      if(not for_) throw_error(for_);

      auto lparen = parse_token('(') | format_error("{} after 'for'");
      if(not lparen) throw_error(lparen);

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

      std::optional<expression> maybe_condition;
      if(not match(token::semicolon))
      {
        auto condition = parse_expression();
        if(not condition) throw_error(condition);
        maybe_condition = *condition;
      }

      auto semi = parse_token(';') | format_error("{} after for loop condition");
      if(not semi) throw_error(semi);

      std::optional<expression> maybe_increment;
      if(not match(token::right_paren))
      {
        auto increment = parse_expression();
        if(not increment) throw_error(increment);
        maybe_increment = *increment;
      }

      auto rparen = parse_token(')') | format_error("{} after for loop increment");
      if(not rparen) throw_error(rparen);

      statement body = expect_statement();

      return {initializer, maybe_condition, maybe_increment, body};
    }

    // block_statement := "{" declaration* "}"
    block_statement expect_block_statement()
    {
      auto lbrace = parse_token('{') | format_error("{} before block");
      if(not lbrace) throw_error(lbrace);

      std::vector<statement> stmts;

      while(peek().which_kind() != token::right_brace)
      {
        stmts.push_back(expect_declaration());
      }

      auto rbrace = parse_token('}') | format_error("{} after block");
      if(not rbrace) throw_error(rbrace);

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
      auto var = parse_token(token::var) | format_error("{} before variable name");
      if(not var) throw_error(var);

      auto name = parse_token(token::identifier);
      if(not name) throw_error(name);

      std::optional<expression> maybe_initializer;

      if(match(token::equal))
      {
        auto initializer = parse_expression();
        if(not initializer) throw_error(initializer);
        maybe_initializer = *initializer;
      }

      auto semi = parse_token(';') | format_error("{} after variable declaration");
      if(not semi) throw_error(semi);

      return {*name, maybe_initializer};
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
          auto id = parse_token(token::identifier) | format_error("{} in function parameter list");
          if(not id) throw_error(id);
          result.push_back(*id);
        }
        while(match(token::comma));
      }

      return result;
    }

    // function_declaration := "fun" IDENTIFIER "(" parameters? ")" block_statement
    function_declaration expect_function_declaration()
    {
      auto fun = parse_token(token::fun) | format_error("{} before function name");
      if(not fun) throw_error(fun);

      auto name = parse_token(token::identifier);
      if(not name) throw_error(name);

      auto lparen = parse_token('(') | format_error("{} before function parameters");
      if(not lparen) throw_error(lparen);

      std::vector<token> parameters = expect_parameters();

      auto rparen = parse_token(')') | format_error("{} after function parameters");
      if(not rparen) throw_error(rparen);

      block_statement body = expect_block_statement();

      return {*name, parameters, body};
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
      auto eof = parse_token(token::eof);
      if(not eof) throw_error(eof);

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

