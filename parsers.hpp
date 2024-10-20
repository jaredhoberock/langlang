#pragma once

#include <expected>
#include <format>
#include <ranges>
#include <span>
#include "syntax.hpp"
#include "token_range.hpp"


namespace parsers
{


constexpr token peek(std::span<token> tokens)
{
  if(tokens.empty())
  {
    return token(token::eof, source_location());
  }
  
  return tokens.front();
}

constexpr token advance(std::span<token>& tokens)
{
  token result = peek(tokens);
  
  if(not tokens.empty())
  {
    tokens = std::views::drop(tokens, 1);
  }
  
  return result;
}


template<class T>
struct success
{
  T value;

  // XXX duplicating remaining in both success & failure is really unhelpful so far
  std::span<token> remaining;

  template<class U>
    requires std::constructible_from<T,U&&>
  constexpr success(U&& v, std::span<token> r)
    : value(std::forward<U>(v)), remaining(r)
  {}

  template<class U>
    requires std::constructible_from<T,U>
  constexpr success(const success<U>& other)
    : success(other.value, other.remaining)
  {}
};

template<class T>
success(T&&, std::span<token>) -> success<std::remove_cvref_t<T>>;

struct failure
{
  std::string message;
  std::span<token> remaining;

  template<class T>
  constexpr operator std::expected<success<T>,failure> () const
  {
    return std::unexpected(*this);
  }

  constexpr bool operator<(const failure& other) const
  {
    return remaining.size() < other.remaining.size();
  }
};

template<class T>
using result = std::expected<success<T>,failure>;


// XXX eliminate this once we've rehauled the parser class
template<class T>
constexpr std::expected<T,std::string> to_expected_or_message(result<T> r)
{
  return r
    .transform([](success<T> s)
    {
      return s.value;
    })
    .transform_error([](failure f)
    {
      return f.message;
    })
  ;
}


template<class T, std::invocable<T> F>
constexpr auto operator|(result<T>&& r, F f)
{
  return std::move(r).transform([&](auto&& result)
  {
    return success{f(std::move(result.value)), result.remaining};
  });
}


struct format_error_t
{
  const char* fmt;

  constexpr failure operator()(failure&& fail) const
  {
    std::string formatted_message = std::vformat(fmt, std::make_format_args(fail.message));
    return {formatted_message, fail.remaining};
  }
};

constexpr format_error_t format_error(const char* fmt)
{
  return {fmt};
}

template<class T>
constexpr auto operator|(result<T>&& r, format_error_t xfrm)
{
  return std::move(r).transform_error(xfrm);
}


constexpr bool failed_equivalently(failure fail0, failure fail1)
{
  return fail0.remaining.size() == fail1.remaining.size();
}


template<std::same_as<failure>... Fs>
constexpr bool failed_equivalently(failure fail0, failure fail1, Fs... fails)
{
  return failed_equivalently(fail0, fail1) and (... and failed_equivalently(fail0, fails));
}


constexpr failure failed_least(failure fail0, failure fail1)
{
  // return the failure that made the most parsing progress
  if(fail1.remaining.size() < fail0.remaining.size()) return fail1;
  return fail0;
}

template<std::same_as<failure>... Failures>
constexpr failure failed_least(failure fail0, failure fail1, Failures... fails)
{
  return failed_least(fail0, failed_least(fail1, fails...));
}


template<std::same_as<failure>... Fs>
constexpr failure failed_in_context(std::string_view context, failure fail, Fs... fails)
{
  failure combined = failed_least(fail, fails...);

  // if no parser was the clear winner, there's no reason to include any individual failure's message
  if(failed_equivalently(fail, fails...))
  {
    return failure(std::format("expected {}", context), combined.remaining);
  }

  return failure(std::format("{} in {}", combined.message, context), combined.remaining);
}


constexpr result<token> parse_token(token::kind k, std::span<token> input)
{
  if(peek(input).which_kind() != k)
  {
    return failure{std::format("Expected '{}'", token::to_string(k)), input};
  }

  token tok = advance(input);
  return success{tok, input};
}

constexpr result<token> parse_token(char c, std::span<token> input)
{
  return parse_token(token::to_kind(c), input);
}

constexpr result<token> parse_any_token(std::span<token> input)
{
  return failure{"No possible token to parse", input};
}

template<class T>
concept char_or_token_kind = std::same_as<char,T> or std::same_as<token::kind,T>;

template<char_or_token_kind T, char_or_token_kind... Ts>
constexpr result<token> parse_any_token(std::span<token> input, T t0, Ts... ts)
{
  auto r0 = parse_token(t0, input);
  if(r0) return r0;
  
  auto r1 = parse_any_token(input, ts...);
  if(r1) return r1;

  return failed_least(r0.error(), r1.error());
}

constexpr result<literal> parse_number(std::span<token> input)
{
  return parse_token(token::number, input) | [](token t)
  {
    return literal{t.number_literal()};
  };
}

constexpr result<literal> parse_string(std::span<token> input)
{
  return parse_token(token::string, input) | [](token t)
  {
    return literal{t.string_literal()};
  };
}

constexpr result<literal> parse_true(std::span<token> input)
{
  return parse_token(token::true_, input) | [](token)
  {
    return literal{true};
  };
}

constexpr result<literal> parse_false(std::span<token> input)
{
  return parse_token(token::false_, input) | [](token)
  {
    return literal{false};
  };
}

constexpr result<literal> parse_nil(std::span<token> input)
{
  return parse_token(token::nil, input) | [](token)
  {
    return literal{nullptr};
  };
}


constexpr result<literal> parse_literal(std::span<token> input)
{
  auto number = parse_number(input);
  if(number) return number;

  auto string = parse_string(input);
  if(string) return string;

  auto true_ = parse_true(input);
  if(true_) return true_;

  auto false_ = parse_false(input);
  if(false_) return false_;

  auto nil = parse_nil(input);
  if(nil) return nil;

  return failed_least(number.error(), string.error(), true_.error(), false_.error(), nil.error());
}


constexpr result<token> parse_identifier(std::span<token> input)
{
  return parse_token(token::identifier, input);
}


constexpr result<variable> parse_variable(std::span<token> input)
{
  return parse_identifier(input) | [](token t)
  {
    return variable{t};
  };
}


constexpr result<expression> parse_expression(std::span<token> input);


// grouping_expression := '(' expression ')'
constexpr result<grouping_expression> parse_grouping_expression(std::span<token> input)
{
  auto lparen = parse_token('(', input) | format_error("{} before expression");
  if(not lparen) return lparen.error();

  auto expr = parse_expression(lparen->remaining) | format_error("{} after '('");
  if(not expr) return expr.error();

  auto rparen = parse_token(')', expr->remaining) | format_error("{} after expression");
  if(not rparen) return rparen.error();

  grouping_expression grouping_expr(lparen->value, expr->value, rparen->value);
  return success(grouping_expr, rparen->remaining);
}


// primary_expression := literal | grouping_expression | variable
constexpr result<expression> parse_primary_expression(std::span<token> input)
{
  auto literal = parse_literal(input);
  if(literal) return literal;

  auto expr = parse_grouping_expression(input);
  if(expr) return expr;

  auto identifier = parse_variable(input);
  if(identifier) return identifier;

  return failed_in_context("primary expression", literal.error(), expr.error(), identifier.error());
}


// arguments := expression ( "," expression )*
constexpr result<std::vector<expression>> parse_arguments(std::span<token> input)
{
  std::vector<expression> exprs;

  while(true)
  {
    if(exprs.size() == 255)
    {
      return failure("Can't have more than 255 arguments", input);
    }

    auto err_fmt_str = exprs.size() > 0 ? "{} after ',' in argument list" : "{}";

    auto expr = parse_expression(input) | format_error(err_fmt_str);
    if(not expr) return expr.error();
    input = expr->remaining;

    exprs.push_back(expr->value);

    auto comma = parse_token(',', input);
    if(not comma) break;
    input = comma->remaining;
  }

  return success(exprs, input);
}


// assert := "assert" expression ";"
constexpr result<assert_statement> parse_assert_statement(std::span<token> input)
{
  auto assert = parse_token(token::assert, input) | format_error("{} before expression");
  if(not assert) return assert.error();

  auto expr = parse_expression(assert->remaining);
  if(not expr) return expr.error();

  auto semi = parse_token(';', expr->remaining) | format_error("{} after assert expression");
  if(not semi) return expr.error();

  return success(assert_statement(expr->value), semi->remaining);
}


// call := primary_expression ( "(" arguments? ")" )*
constexpr result<expression> parse_call(std::span<token> input)
{
  auto r = parse_primary_expression(input);
  if(not r) return r;

  while(true)
  {
    auto lparen = parse_token('(', r->remaining);
    if(not lparen) break;

    if(auto rparen = parse_token(')', lparen->remaining))
    {
      call_expression call(r->value, std::vector<expression>(), rparen->value);
      r = success(call, rparen->remaining);
      break;
    }

    auto args = parse_arguments(lparen->remaining) | format_error("{} after '('");
    if(not args) return args.error();

    auto rparen = parse_token(')', args->remaining) | format_error("{} after arguments");
    if(not rparen) return rparen.error();

    call_expression call(r->value, args->value, rparen->value);
    r = success(call, rparen->remaining);
  }

  return r;
}


// unary := ( "!" | "-" ) unary | call
constexpr result<expression> parse_unary(std::span<token> input)
{
  if(auto op = parse_any_token(input, '!', '-'))
  {
    return parse_unary(op->remaining) | [&](expression&& result)
    {
      return unary_expression(op->value, std::move(result));
    };
  }

  return parse_call(input);
}


// factor := unary ( ( "/" | "*" ) ) unary )*
constexpr result<expression> parse_factor(std::span<token> input)
{
  auto r = parse_unary(input);
  if(not r) return r;

  while(auto op = parse_any_token(r->remaining, '/', '*'))
  {
    auto right = parse_unary(op->remaining);
    if(not right) return right;

    binary_expression bin_expr(r->value, op->value, right->value);
    r = success(bin_expr, right->remaining);
  }

  return r;
}


// term := factor ( ( "-" | "+" ) factor )*
constexpr result<expression> parse_term(std::span<token> input)
{
  auto r = parse_factor(input);
  if(not r) return r;

  while(auto op = parse_any_token(r->remaining, '-', '+'))
  {
    auto right = parse_factor(op->remaining);
    if(not right) return right;

    binary_expression bin_expr(r->value, op->value, right->value);
    r = success(bin_expr, right->remaining);
  }

  return r;
}


// comparison := term ( ( ">" | ">=" | "<" | "<=" ) term )*
constexpr result<expression> parse_comparison(std::span<token> input)
{
  auto r = parse_term(input);
  if(not r) return r;

  while(auto op = parse_any_token(r->remaining,
                                  token::greater, token::greater_equal,
                                  token::less, token::less_equal))
  {
    auto right = parse_term(op->remaining);
    if(not right) return right;

    binary_expression bin_expr(std::move(r->value), std::move(op->value), std::move(right->value));
    r = success(std::move(bin_expr), right->remaining);
  }

  return r;
}


// equality := comparison ( ( "!=" | "==" ) comparison )*
constexpr result<expression> parse_equality(std::span<token> input)
{
  auto r = parse_comparison(input);
  if(not r) return r;

  while(auto op = parse_any_token(r->remaining, token::bang_equal, token::equal_equal))
  {
    auto right = parse_comparison(op->remaining);
    if(not right) return right;

    binary_expression expr(r->value, op->value, right->value);
    r = success(expr, right->remaining);
  }

  return r;
}


// logical_and := equality ( "and" equality)*
constexpr result<expression> parse_logical_and(std::span<token> input)
{
  auto r = parse_equality(input);
  if(not r) return r;

  while(auto op = parse_token(token::and_, r->remaining))
  {
    auto right = parse_equality(op->remaining);
    if(not right) return right;

    logical_expression expr(r->value, op->value, right->value);
    r = success(expr, right->remaining);
  }

  return r;
}


// logical_or := equality ( "or" equality)*
constexpr result<expression> parse_logical_or(std::span<token> input)
{
  auto r = parse_logical_and(input);
  if(not r) return r;

  while(auto op = parse_token(token::or_, r->remaining))
  {
    auto right = parse_equality(op->remaining);
    if(not right) return right;

    logical_expression expr(r->value, op->value, right->value);
    r = success(expr, right->remaining);
  }

  return r;
}


// assignment := logical_or | identifier "=" assignment
constexpr result<expression> parse_assignment(std::span<token> input)
{
  auto r = parse_logical_or(input);
  if(not r) return r;

  if(auto op = parse_token(token::equal, r->remaining))
  {
    if(not std::holds_alternative<variable>(r->value))
    {
      return failure{"Invalid assignment target", op->remaining};
    }

    auto rhs = parse_assignment(op->remaining);
    if(not rhs) return rhs;

    assignment_expression expr(get<variable>(r->value), std::move(rhs->value));
    r = success(expr, rhs->remaining);
  }

  return r;
}


// expression := assignment
constexpr result<expression> parse_expression(std::span<token> input)
{
  return parse_assignment(input);
}


// variable_declaration := "var" identifier ( "=" expression )? ";"
constexpr result<variable_declaration> parse_variable_declaration(std::span<token> input)
{
  auto var = parse_token(token::var, input) | format_error("{} before variable name");
  if(not var) return var.error();

  auto name = parse_identifier(var->remaining);
  if(not name) return name.error();

  input = name->remaining;
  std::optional<expression> maybe_init;

  if(auto op = parse_token('=', input))
  {
    auto init = parse_expression(op->remaining);
    if(not init) return init.error();

    maybe_init = init->value;
    input = init->remaining;
  }

  auto semi = parse_token(';', input) | format_error("{} after variable declaration");
  if(not semi) return semi.error();

  variable_declaration decl(name->value, maybe_init);
  return success(decl, semi->remaining);
}


// expression_statement := expression ";"
constexpr result<expression_statement> parse_expression_statement(std::span<token> input)
{
  auto expr = parse_expression(input);
  if(not expr) return expr.error();

  auto semi = parse_token(';', expr->remaining) | format_error("{} after expression");
  if(not semi) return semi.error();

  expression_statement stmt(expr->value);
  return success(stmt, semi->remaining);
}


// print_statement := "print" expression ";"
constexpr result<print_statement> parse_print_statement(std::span<token> input)
{
  auto print = parse_token(token::print, input) | format_error("{} before expression");
  if(not print) return print.error();

  auto expr = parse_expression(print->remaining);
  if(not expr) return expr.error();

  auto semi = parse_token(';', expr->remaining) | format_error("{} after print expression");
  if(not semi) return semi.error();

  return success(print_statement(expr->value), semi->remaining);
}


constexpr result<statement> parse_statement(std::span<token>);


// if_statement := "if" "(" expression ")" statement ( "else" statement )?
constexpr result<if_statement> parse_if_statement(std::span<token> input)
{
  auto if_ = parse_token(token::if_, input) | format_error("{} before condition");
  if(not if_) return if_.error();

  auto lparen = parse_token('(', if_->remaining) | format_error("{} after 'if'");
  if(not lparen) return lparen.error();

  auto expr = parse_expression(lparen->remaining);
  if(not expr) return expr.error();

  auto rparen = parse_token(')', expr->remaining) | format_error("{} after if condition");
  if(not rparen) return rparen.error();

  auto then_branch = parse_statement(rparen->remaining);
  if(not then_branch) return then_branch.error();

  input = then_branch->remaining;
  std::optional<statement> maybe_else_branch;

  if(auto else_ = parse_token(token::else_, input))
  {
    auto else_branch = parse_statement(else_->remaining);
    if(not else_branch) return else_branch.error();

    maybe_else_branch = else_branch->value;
    input = else_branch->remaining;
  }

  if_statement stmt(expr->value, then_branch->value, maybe_else_branch);
  return success(stmt, input);
}


// return_statement := "return" expression? ";"
constexpr result<return_statement> parse_return_statement(std::span<token> input)
{
  auto ret = parse_token(token::return_, input) | format_error("{} before expression");
  if(not ret) return ret.error();

  if(auto semi = parse_token(';', ret->remaining))
  {
    return_statement stmt{ret->value, std::nullopt};
    return result<return_statement>(std::in_place, stmt, semi->remaining);
  }

  auto expr = parse_expression(ret->remaining) | format_error("{} after 'return'");
  if(not expr) return expr.error();

  auto semi = parse_token(';', expr->remaining) | format_error("{} after return value");
  if(not semi) return semi.error();

  return_statement stmt{ret->value, expr->value};
  return success(stmt, semi->remaining);
}


// while_statement := "while" "(" expression ")" statement
constexpr result<while_statement> parse_while_statement(std::span<token> input)
{
  auto while_ = parse_token(token::while_, input) | format_error("{} before condition");
  if(not while_) return while_.error();

  auto lparen = parse_token('(', while_->remaining) | format_error("{} after 'while'");
  if(not lparen) return lparen.error();

  auto expr = parse_expression(lparen->remaining);
  if(not expr) return expr.error();

  auto rparen = parse_token(')', expr->remaining);
  if(not rparen) return rparen.error();

  auto body = parse_statement(rparen->remaining);
  if(not body) return body.error();

  while_statement stmt(expr->value, body->value);
  return success(stmt, body->remaining);
}


// for_statement := "for" "(" ( variable_declaration | expression_statement ) expression? ";" expression? ")" statement
constexpr result<for_statement> parse_for_statement(std::span<token> input)
{
  auto for_ = parse_token(token::for_, input) | format_error("{} before loop");
  if(not for_) return for_.error();

  auto lparen = parse_token('(', for_->remaining) | format_error("{} after 'for'");
  if(not lparen) return lparen.error();

  input = lparen->remaining;

  std::optional<statement> maybe_init;
  if(auto init = parse_variable_declaration(input))
  {
    maybe_init = init->value;
    input = init->remaining;
  }
  else if(auto init = parse_expression_statement(input))
  {
    maybe_init = init->value;
    input = init->remaining;
  }

  std::optional<expression> maybe_condition;
  if(auto condition = parse_expression(input))
  {
    maybe_condition = condition->value;
    input = condition->remaining;
  }

  auto semi = parse_token(';', input) | format_error("{} after for loop condition");
  if(not semi) return semi.error();

  input = semi->remaining;

  std::optional<expression> maybe_increment;
  if(auto increment = parse_expression(input))
  {
    maybe_increment = increment->value;
    input = increment->remaining;
  }

  auto rparen = parse_token(')', input) | format_error("{} after for loop increment");
  if(not rparen) return rparen.error();

  auto body = parse_statement(rparen->remaining);
  if(not body) return body.error();

  for_statement stmt(maybe_init, maybe_condition, maybe_increment, body->value);
  return success(stmt, body->remaining);
}


constexpr result<statement> parse_declaration(std::span<token> input);


// block_statement := "{" declaration* "}"
constexpr result<block_statement> parse_block_statement(std::span<token> input)
{
  auto lbrace = parse_token('{', input) | format_error("{} before block");
  if(not lbrace) return lbrace.error();

  std::span<token> remaining = lbrace->remaining;
  std::vector<statement> stmts;

  while(true)
  {
    auto rbrace = parse_token('}', remaining);
    if(rbrace)
    {
      remaining = rbrace->remaining;
      break;
    }

    auto decl = parse_declaration(remaining);
    if(not decl) return decl.error();
    
    stmts.push_back(decl->value);
    remaining = decl->remaining;
  }

  return success(block_statement(stmts), remaining);
}


// statement := assert_statement | expression_statement | print_statement | if_statement | return_statement | while_statement | for_statement | block_statement
constexpr result<statement> parse_statement(std::span<token> input)
{
  auto assert = parse_assert_statement(input);
  if(assert) return assert;

  auto expr = parse_expression_statement(input);
  if(expr) return expr;

  auto print = parse_print_statement(input);
  if(print) return print;

  auto if_ = parse_if_statement(input);
  if(if_) return if_;

  auto ret = parse_return_statement(input);
  if(ret) return ret;

  auto while_ = parse_while_statement(input);
  if(while_) return while_;

  auto for_ = parse_for_statement(input);
  if(for_) return for_;

  auto block = parse_block_statement(input);
  if(block) return block;

  return failed_least(assert.error(), expr.error(), print.error(), if_.error(), ret.error(), while_.error(), for_.error(), block.error());
}


// parameters := identifier ( "," identifier )*
constexpr result<std::vector<token>> parse_parameters(std::span<token> input)
{
  auto parm0 = parse_identifier(input);
  if(not parm0) return parm0.error();

  std::vector<token> parms = {parm0->value};
  std::span<token> remaining = parm0->remaining;

  while(true)
  {
    auto comma = parse_token(',', remaining);
    if(not comma)
    {
      break;
    }

    auto parm = parse_identifier(comma->remaining) | format_error("{} in function parameter list");
    if(not parm) return parm.error();

    parms.push_back(parm->value);
    remaining = parm->remaining;
  }

  return success(parms, remaining);
}


// function_declaration := "fun" identifier "(" parameters? ")" block_statement
constexpr result<function_declaration> parse_function_declaration(std::span<token> input)
{
  auto fun = parse_token(token::fun, input) | format_error("{} before function name");
  if(not fun) return fun.error();

  auto name = parse_identifier(fun->remaining);
  if(not name) return name.error();

  auto lparen = parse_token('(', name->remaining) | format_error("{} before function parameters");
  if(not lparen) return lparen.error();

  std::vector<token> parameters;
  input = lparen->remaining;

  if(auto parms = parse_parameters(input))
  {
    parameters = parms->value;
    input = parms->remaining;
  }

  auto rparen = parse_token(')', input) | format_error("{} after function parameters");
  if(not rparen) return rparen.error();

  auto body = parse_block_statement(rparen->remaining);
  if(not body) return body.error();

  return success(function_declaration(name->value, parameters, body->value), body->remaining);
}


// declaration := function_declaration | variable_declaration | statement
constexpr result<statement> parse_declaration(std::span<token> input)
{
  auto fun = parse_function_declaration(input);
  if(fun) return fun;

  auto var = parse_variable_declaration(input);
  if(var) return var;

  auto stmt = parse_statement(input);
  if(stmt) return stmt;

  return failed_least(fun.error(), var.error(), stmt.error());
}


// program := declaration* EOF
constexpr result<program> parse_program(std::span<token> input)
{
  std::vector<statement> stmts;

  while(true)
  {
    if(auto eof = parse_token(token::eof, input))
    {
      input = eof->remaining;
      break;
    }

    auto decl = parse_declaration(input);
    if(not decl) return decl.error();

    stmts.push_back(decl->value);
    input = decl->remaining;
  }

  return success(program(stmts), input);
}


} // end parsers

