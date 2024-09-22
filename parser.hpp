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
    std::expected<expression,std::string> parse_term()
    {
      auto result = parse_factor();
      if(not result) return result;

      while(std::optional op = match_any(token::minus, token::plus))
      {
        auto right = parse_factor();
        if(not right) return right;

        result = binary_expression{*result, *op, *right};
      }

      return result;
    }

    // comparison := term ( ( ">" | ">=" | "<=" ) term )*
    std::expected<expression,std::string> parse_comparison()
    {
      auto result = parse_term(); 
      if(not result) return result;

      while(std::optional op = match_any(token::greater, token::greater_equal, token::less, token::less_equal))
      {
        auto right = parse_term();
        if(not right) return right;

        result = binary_expression{*result, *op, *right};
      }

      return result;
    }

    // equality := comparison ( ( "!=" | "==" ) comparison )*
    std::expected<expression,std::string> parse_equality()
    {
      auto result = parse_comparison();
      if(not result) return result;

      while(std::optional op = match_any(token::bang_equal, token::equal_equal))
      {
        auto right = parse_comparison();
        if(not right) return right;

        result = binary_expression{*result, *op, *right};
      }

      return result;
    }

    // logical_and := equality ( "and" equality )*
    std::expected<expression,std::string> parse_logical_and()
    {
      auto result = parse_equality();
      if(not result) return result;

      if(std::optional and_ = match(token::and_))
      {
        auto right = parse_equality();
        if(not right) return right;

        result = logical_expression{*result, *and_, *right};
      }

      return result;
    }

    // logical_or := logical_and ( "or" logical_and )*
    std::expected<expression,std::string> parse_logical_or()
    {
      auto result = parse_logical_and();
      if(not result) return result;

      if(std::optional or_ = match(token::or_))
      {
        auto right = parse_logical_and();
        if(not right) return right;

        result = logical_expression{*result, *or_, *right};
      }

      return result;
    }

    // assignment := IDENTIFIER "=" assignment | logical_or
    std::expected<expression,std::string> parse_assignment()
    {
      auto result = parse_logical_or();
      if(not result) return result;

      if(std::optional eq = match(token::equal))
      {
        if(not std::holds_alternative<variable>(*result))
        {
          throw_error(*eq, "Invalid assignment target.");
        }

        auto rhs = parse_assignment();
        if(not rhs) return rhs;

        result = assignment_expression{get<variable>(*result), *rhs};
      }

      return *result;
    }

    // expression := assignment
    std::expected<expression,std::string> parse_expression()
    {
      return parse_assignment();
    }

    // expression_statement := expression ";"
    std::expected<expression_statement,std::string> parse_expression_statement()
    {
      auto expr = parse_expression();
      if(not expr) return std::unexpected(expr.error());

      auto semi = parse_token(';') | format_error("{} after expression");
      if(not semi) return std::unexpected(semi.error());

      return expression_statement{*expr};
    }

    // print_statement := "print" expression ";"
    std::expected<print_statement,std::string> parse_print_statement()
    {
      auto print = parse_token(token::print) | format_error("{} before expression");
      if(not print) return std::unexpected(print.error());

      auto expr = parse_expression();
      if(not expr) return std::unexpected(expr.error());

      auto semi = parse_token(';') | format_error("{} after print expression");
      if(not semi) return std::unexpected(semi.error());

      return print_statement{*expr};
    }

    // if_statement := "if" "(" expression ")" statement ( "else" statement )?
    std::expected<if_statement,std::string> parse_if_statement()
    {
      auto if_ = parse_token(token::if_) | format_error("{} before condition");
      if(not if_) return std::unexpected(if_.error());

      auto lparen = parse_token('(') | format_error("{} after 'if'");
      if(not lparen) return std::unexpected(lparen.error());

      auto expr = parse_expression();
      if(not expr) return std::unexpected(expr.error());

      auto rparen = parse_token(')') | format_error("{} after if condition");
      if(not rparen) return std::unexpected(rparen.error());

      auto then_branch = parse_statement();
      if(not then_branch) return std::unexpected(then_branch.error());

      std::optional<statement> maybe_else_branch;

      if(match(token::else_))
      {
        auto else_branch = parse_statement();
        if(not else_branch) return std::unexpected(else_branch.error());
        maybe_else_branch = *else_branch;
      }

      return if_statement{*expr, *then_branch, maybe_else_branch};
    }

    // return_statement := "return" expression? ";"
    std::expected<return_statement,std::string> parse_return_statement()
    {
      auto ret = parse_token(token::return_) | format_error("{} before expression");
      if(not ret) return std::unexpected(ret.error());

      std::optional<expression> maybe_expr;

      if(peek().which_kind() != token::semicolon)
      {
        auto expr = parse_expression();
        if(not expr) return std::unexpected(expr.error());
        maybe_expr = *expr;
      }

      auto semi = parse_token(';') | format_error("{} after return value");
      if(not semi) return std::unexpected(semi.error());

      return return_statement{*ret, maybe_expr};
    }

    // while_statement := "while" "(" expression ")" statement
    std::expected<while_statement,std::string> parse_while_statement()
    {
      auto while_ = parse_token(token::while_) | format_error("{} before condition");
      if(not while_) return std::unexpected(while_.error());

      auto lparen = parse_token('(') | format_error("{} after 'while'.");
      if(not lparen) return std::unexpected(lparen.error());

      auto expr = parse_expression();
      if(not expr) return std::unexpected(expr.error());

      auto rparen = parse_token(')') | format_error("{} after while condition.");
      if(not rparen) return std::unexpected(rparen.error());

      auto body = parse_statement();
      if(not body) return std::unexpected(body.error());

      return while_statement{*expr, *body};
    }

    // for_statement := "for" "(" ( variable_declaration | expression_statement ) expression? ";" expression? ")" statement
    std::expected<for_statement,std::string> parse_for_statement()
    {
      auto for_ = parse_token(token::for_) | format_error("{} before loop.");
      if(not for_) return std::unexpected(for_.error());

      auto lparen = parse_token('(') | format_error("{} after 'for'");
      if(not lparen) return std::unexpected(lparen.error());

      std::optional<statement> maybe_init;
      if(not match(token::semicolon))
      {
        if(peek().which_kind() == token::var)
        {
          auto init = parse_variable_declaration();
          if(not init) return std::unexpected(init.error());
          maybe_init = *init;
        }
        else
        {
          auto init = parse_expression_statement();
          if(not init) return std::unexpected(init.error());
          maybe_init = *init;
        }
      }

      std::optional<expression> maybe_condition;
      if(not match(token::semicolon))
      {
        auto condition = parse_expression();
        if(not condition) return std::unexpected(condition.error());
        maybe_condition = *condition;
      }

      auto semi = parse_token(';') | format_error("{} after for loop condition");
      if(not semi) return std::unexpected(semi.error());

      std::optional<expression> maybe_increment;
      if(not match(token::right_paren))
      {
        auto increment = parse_expression();
        if(not increment) return std::unexpected(increment.error());
        maybe_increment = *increment;
      }

      auto rparen = parse_token(')') | format_error("{} after for loop increment");
      if(not rparen) return std::unexpected(rparen.error());

      auto body = parse_statement();
      if(not body) return std::unexpected(body.error());

      return for_statement{maybe_init, maybe_condition, maybe_increment, *body};
    }

    // block_statement := "{" declaration* "}"
    std::expected<block_statement,std::string> parse_block_statement()
    {
      auto lbrace = parse_token('{') | format_error("{} before block");
      if(not lbrace) return std::unexpected(lbrace.error());

      std::vector<statement> stmts;

      while(peek().which_kind() != token::right_brace)
      {
        auto decl = parse_declaration();
        if(not decl) return std::unexpected(decl.error());
        stmts.push_back(*decl);
      }

      auto rbrace = parse_token('}') | format_error("{} after block");
      if(not rbrace) return std::unexpected(rbrace.error());

      return block_statement{stmts};
    }

    // statement := expression_statement | print_statement | if_statement | return_statement | while_statement | for_statement | "{" declaration* "}"
    std::expected<statement,std::string> parse_statement()
    {
      if(peek().which_kind() == token::print)
      {
        return parse_print_statement();
      }
      else if(peek().which_kind() == token::if_)
      {
        return parse_if_statement();
      }
      else if(peek().which_kind() == token::return_)
      {
        return parse_return_statement();
      }
      else if(peek().which_kind() == token::while_)
      {
        return parse_while_statement();
      }
      else if(peek().which_kind() == token::for_)
      {
        return parse_for_statement();
      }
      else if(peek().which_kind() == token::left_brace)
      {
        return  parse_block_statement();
      }

      return parse_expression_statement();
    }

    // variable_declaration := "var" IDENTIFIER ( "=" expression )? ";"
    std::expected<variable_declaration,std::string> parse_variable_declaration()
    {
      auto var = parse_token(token::var) | format_error("{} before variable name");
      if(not var) return std::unexpected(var.error());

      auto name = parse_token(token::identifier);
      if(not name) return std::unexpected(name.error());

      std::optional<expression> maybe_initializer;

      if(match(token::equal))
      {
        auto initializer = parse_expression();
        if(not initializer) return std::unexpected(initializer.error());
        maybe_initializer = *initializer;
      }

      auto semi = parse_token(';') | format_error("{} after variable declaration");
      if(not semi) return std::unexpected(semi.error());

      return variable_declaration{*name, maybe_initializer};
    }

    // parameters := IDENTIFIER ( "," IDENTIFIER )*
    std::expected<std::vector<token>,std::string> parse_parameters()
    {
      std::vector<token> result;

      if(peek().which_kind() != token::right_paren)
      {
        if(result.size() == 255)
        {
          return std::unexpected("Can't have more than 255 parameters");
        }

        do
        {
          auto id = parse_token(token::identifier) | format_error("{} in function parameter list");
          if(not id) return std::unexpected(id.error());
          result.push_back(*id);
        }
        while(match(token::comma));
      }

      return result;
    }

    // function_declaration := "fun" IDENTIFIER "(" parameters? ")" block_statement
    std::expected<function_declaration,std::string> parse_function_declaration()
    {
      auto fun = parse_token(token::fun) | format_error("{} before function name");
      if(not fun) return std::unexpected(fun.error());

      auto name = parse_token(token::identifier);
      if(not name) return std::unexpected(name.error());

      auto lparen = parse_token('(') | format_error("{} before function parameters");
      if(not lparen) return std::unexpected(lparen.error());

      auto parameters = parse_parameters();
      if(not parameters) return std::unexpected(parameters.error());

      auto rparen = parse_token(')') | format_error("{} after function parameters");
      if(not rparen) return std::unexpected(rparen.error());

      auto body = parse_block_statement();
      if(not body) return std::unexpected(body.error());

      return function_declaration{*name, *parameters, *body};
    }

    // declaration := function_declaration | variable_declaration | statement
    std::expected<statement,std::string> parse_declaration()
    {
      if(peek().which_kind() == token::fun)
      {
        return parse_function_declaration();
      }
      else if(peek().which_kind() == token::var)
      {
        return parse_variable_declaration();
      }

      return parse_statement();
    }

    // program := declaration* EOF
    program expect_program()
    {
      std::vector<statement> statements;

      while(peek().which_kind() != token::eof)
      {
        auto decl = parse_declaration();
        if(not decl) throw_error(decl);
        statements.push_back(*decl);
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

