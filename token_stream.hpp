#pragma once

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <fmt/format.h>
#include <fstream>
#include <format>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <string_view>
#include <variant>
#include <vector>
#include "source_location.hpp"

class token
{
  public:
    enum kind
    {
      and_, bang, bang_equal, class_, comma, dot, else_, eof, error,
      equal, equal_equal, false_, for_, fun, greater, greater_equal,
      identifier, if_, left_brace, left_paren, less, less_equal,
      minus, nil, number, or_, plus, print, return_, right_brace,
      right_paren, semicolon, slash, star, string, super, this_,
      true_, var, while_,
    };

    static constexpr kind to_kind(char c)
    {
      if(c == '!') return bang;
      if(c == ',') return comma;
      if(c == '.') return dot;
      if(c == '=') return equal;
      if(c == '>') return greater;
      if(c == '{') return left_brace;
      if(c == '(') return left_paren;
      if(c == '<') return less;
      if(c == '-') return minus;
      if(c == '+') return plus;
      if(c == '}') return right_brace;
      if(c == ')') return right_paren;
      if(c == ';') return semicolon;
      if(c == '/') return slash;
      if(c == '*') return star;
      return error;
    }

    static const char* to_string(kind k)
    {
      switch(k)
      {
        case(and_): return "and";
        case(bang): return "!";
        case(bang_equal): return "!=";
        case(class_): return "class";
        case(comma): return ",";
        case(dot): return ".";
        case(else_): return "else";
        case(eof): return "eof";
        case(error): return "error";
        case(equal): return "=";
        case(equal_equal): return "==";
        case(false_): return "false";
        case(for_): return "for";
        case(fun): return "fun";
        case(greater): return ">";
        case(greater_equal): return ">=";
        case(identifier): return "identifier";
        case(if_): return "if";
        case(left_brace): return "{";
        case(left_paren): return "(";
        case(less): return "<";
        case(less_equal): return "<=";
        case(minus): return "-";
        case(nil): return "nil";
        case(number): return "number";
        case(or_): return "or";
        case(plus): return "+";
        case(print): return "print";
        case(return_): return "return";
        case(right_brace): return "}";
        case(right_paren): return ")";
        case(semicolon): return ";";
        case(slash): return "/";
        case(star): return "*";
        case(string): return "string";
        case(super): return "super";
        case(this_): return "this";
        case(true_): return "true";
        case(var): return "var";
        case(while_): return "while";
      }

      return "unknown";
    }

    token(kind k, std::string lexeme, std::string literal, source_location location)
      : kind_(k), lexeme_(lexeme), maybe_literal_(literal), location_(location)
    {}

    token(kind k, std::string lexeme, double literal, source_location location)
      : kind_(k), lexeme_(lexeme), maybe_literal_(literal), location_(location)
    {}

    token(kind k, std::string lexeme, source_location location)
      : kind_(k), lexeme_(lexeme), maybe_literal_(), location_(location)
    {}

    token(kind k, source_location location)
      : kind_(k), lexeme_(), maybe_literal_(), location_(location)
    {}

    kind which_kind() const
    {
      return kind_;
    }

    source_location location() const
    {
      return location_;
    }

    const char* lexeme() const
    {
      if(not lexeme_.empty())
      {
        return lexeme_.c_str();
      }

      return to_string(which_kind());
    }

    const std::string& string_literal() const
    {
      return get<std::string>(*maybe_literal_);
    }

    double number_literal() const
    {
      return get<double>(*maybe_literal_);
    }

    friend std::ostream& operator<<(std::ostream& os, const token& self)
    {
      os << token::to_string(self.kind_) << " " << self.lexeme() << " ";

      if(self.maybe_literal_.has_value())
      {
        if(std::holds_alternative<std::string>(*self.maybe_literal_))
        {
          os << std::get<std::string>(*self.maybe_literal_);
        }
        else if(std::holds_alternative<double>(*self.maybe_literal_))
        {
          os << std::get<double>(*self.maybe_literal_);
        }

        std::cout << " ";
      }

      os << "line " << self.location().line << " column " << self.location().column;

      return os;
    }

    static kind classify_identifier(const std::string& lexeme)
    {
      kind result = identifier;

      if(keywords_.contains(lexeme))
      {
        result = keywords_.at(lexeme);
      }

      return result;
    }

  private:
    kind kind_;
    std::string lexeme_;
    std::optional<std::variant<std::string,double>> maybe_literal_;
    source_location location_;

    static const std::map<std::string,kind> keywords_;
};


const std::map<std::string,token::kind> token::keywords_ = {
  {"and", and_},
  {"class", class_},
  {"else", else_},
  {"false", false_},
  {"for", for_},
  {"fun", fun},
  {"if", if_},
  {"nil", nil},
  {"or", or_},
  {"print", print},
  {"return", return_},
  {"super", super},
  {"this", this_},
  {"true", true_},
  {"var", var},
  {"while", while_}
};


class token_stream
{
  public:
    token_stream(std::string_view source, source_location loc = {})
      : source_(source), loc_(loc)
    {}

    struct sentinel {};

    class iterator
    {
      public:
        iterator(token_stream& stream)
          : stream_{stream}, current_{stream_.next()}
        {}

        token operator*() const
        {
          return current_;
        }

        iterator& operator++()
        {
          current_ = stream_.next();
          return *this;
        }

        bool operator==(sentinel) const
        {
          // EOF is the final token in the stream
          return current_.which_kind()== token::eof;
        }

      private:
        token_stream& stream_;
        token current_;
    };

    iterator begin()
    {
      return {*this};
    }

    sentinel end() const
    {
      return {};
    }

  private:
    token next()
    {
      // consume characters until either eof the next lexeme has been found
      // this loop exits via return
      while(not is_at_end())
      {
        source_location c_loc = loc_;
        char c = advance();

        switch(c)
        {
          // single-character lexemes
          case '(': return {token::left_paren, c_loc};
          case ')': return {token::right_paren, c_loc};
          case '{': return {token::left_brace, c_loc};
          case '}': return {token::right_brace, c_loc};
          case ',': return {token::comma, c_loc};
          case '.': return {token::dot, c_loc};
          case '-': return {token::minus, c_loc};
          case '+': return {token::plus, c_loc};
          case ';': return {token::semicolon, c_loc};
          case '*': return {token::star, c_loc};

          // possibly double-character lexemes
          case '!': return { advance_if_matches('=') ? token::bang_equal : token::bang, c_loc};
          case '=': return { advance_if_matches('=') ? token::equal_equal : token::equal, c_loc};
          case '<': return { advance_if_matches('=') ? token::less_equal : token::less, c_loc};
          case '>': return { advance_if_matches('=') ? token::greater_equal : token::greater, c_loc};

          // newline
          case '\n':
          {
            loc_.advance_line();
            break;
          }

          // slash
          case '/':
          {
            // check for a comment
            if(advance_if_matches('/'))
            {
              // consume until the end of the line
              while(peek() != '\n' and not is_at_end())
              {
                advance();
              }
            }
            else
            {
              return {token::slash, c_loc};
            }

            break;
          }

          // whitespace
          case ' ':
          case '\r':
          case '\t':
          {
            break;
          }

          // string literals
          case '"':
          {
            return expect_terminated_string(c_loc);
          }

          default:
          {
            if(std::isdigit(c))
            {
              return expect_number(c);
            }
            else if(c == '_' or std::isalpha(c))
            {
              return expect_identifier(c, c_loc);
            }

            return {token::error, c_loc};
          }
        }
      }

      return {token::eof, loc_};
    }

    bool is_at_end() const
    {
      return source_.empty();
    }

    char peek() const
    {
      if(source_.empty()) return '\0';
      return source_.front();
    }

    char peek_next() const
    {
      if(source_.size() < 2) return '\0';
      return source_[1];
    }

    bool advance_if_matches(char expected)
    {
      bool result = (not source_.empty()) and source_.front() == expected;

      if(result)
      {
        advance();
      }

      return result;
    }

    char advance()
    {
      assert(not is_at_end());
      char result = source_.front();
      source_.remove_prefix(1);
      loc_.advance_column();
      return result;
    }

    token expect_terminated_string(source_location open_quote_loc)
    {
      std::string lexeme;

      while(peek() != '"' and not is_at_end())
      {
        if(peek() == '\n')
        {
          loc_.advance_line();
        }

        lexeme.push_back(advance());
      }

      if(is_at_end())
      {
        return {token::error, "Unterminated string", open_quote_loc};
      }

      // consume terminating quote
      advance();

      // XXX we'd handle escape sequences here
      std::string literal = lexeme;

      return {token::string, lexeme, literal, open_quote_loc};
    }

    token expect_number(char first_digit)
    {
      std::string lexeme;
      source_location starting_loc = loc_;
      lexeme.push_back(first_digit);

      while(std::isdigit(peek()))
      {
        lexeme.push_back(advance());
      }

      if(peek() == '.' and std::isdigit(peek_next()))
      {
        // consume the decimal
        lexeme.push_back(advance());

        // consume the fractional part
        while(std::isdigit(peek()))
        {
          lexeme.push_back(advance());
        }
      }

      double literal = std::atof(lexeme.c_str());

      return {token::number, lexeme, literal, starting_loc};
    }

    token expect_identifier(char first_character, source_location first_loc)
    {
      std::string lexeme;
      lexeme.push_back(first_character);

      auto is_ok = [](char c)
      {
        return ('a' <= c and c <= 'z') or
               ('A' <= c and c <= 'Z') or
               ('0' <= c and c <= '9') or
               (c == '_');
      };

      while(is_ok(peek()))
      {
        lexeme.push_back(advance());
      }

      return {token::classify_identifier(lexeme), lexeme, first_loc};
    }

    std::string_view source_;
    source_location loc_;
};


template<>
struct fmt::formatter<token>
{
  template<class ParseContext>
  constexpr auto parse(ParseContext& ctx) const
  {
    return ctx.begin();
  }

  template<class FormatContext>
  constexpr auto format(const token& t, FormatContext& ctx) const
  {
    return fmt::format_to(ctx.out(), "{} {} {}", t.which_kind(), t.lexeme(), t.location());
  }
};


template<>
struct std::formatter<token>
{
  template<class ParseContext>
  constexpr auto parse(ParseContext& ctx) const
  {
    return ctx.begin();
  }

  template<class FormatContext>
  constexpr auto format(const token& t, FormatContext& ctx) const
  {
    return std::format_to(ctx.out(), "{} {} {}", t.which_kind(), t.lexeme(), t.location());
  }
};

