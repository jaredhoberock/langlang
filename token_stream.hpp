#pragma once

#include <cassert>
#include <cctype>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <map>
#include <optional>
#include <sstream>
#include <string_view>
#include <variant>
#include <vector>

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

    token(kind k, std::string lexeme, std::string literal, int line_number)
      : kind_(k), lexeme_(lexeme), maybe_literal_(literal), line_number_(line_number)
    {}

    token(kind k, std::string lexeme, double literal, int line_number)
      : kind_(k), lexeme_(lexeme), maybe_literal_(literal), line_number_(line_number)
    {}

    token(kind k, std::string lexeme, int line_number)
      : kind_(k), lexeme_(lexeme), maybe_literal_(), line_number_(line_number)
    {}

    token(kind k, int line_number)
      : kind_(k), lexeme_(), maybe_literal_(), line_number_(line_number)
    {}

    kind which_kind() const
    {
      return kind_;
    }

    int line_number() const
    {
      return line_number_;
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

      os << "line " << self.line_number_;

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
    int line_number_;

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
    token_stream(std::string_view source, int line_number = 0)
      : source_(source), line_number_(line_number)
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
        char c = advance();

        switch(c)
        {
          // single-character lexemes
          case '(': return {token::left_paren, line_number_};
          case ')': return {token::right_paren, line_number_};
          case '{': return {token::left_brace, line_number_};
          case '}': return {token::right_brace, line_number_};
          case ',': return {token::comma, line_number_};
          case '.': return {token::dot, line_number_};
          case '-': return {token::minus, line_number_};
          case '+': return {token::plus, line_number_};
          case ';': return {token::semicolon, line_number_};
          case '*': return {token::star, line_number_};

          // possibly double-character lexemes
          case '!': return { advance_if_matches('=') ? token::bang_equal : token::bang, line_number_};
          case '=': return { advance_if_matches('=') ? token::equal_equal : token::equal, line_number_};
          case '<': return { advance_if_matches('=') ? token::less_equal : token::less, line_number_};
          case '>': return { advance_if_matches('=') ? token::greater_equal : token::greater, line_number_};

          // newline
          case '\n':
          {
            ++line_number_;
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
              return {token::slash, line_number_};
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
            return expect_terminated_string();
          }

          default:
          {
            if(std::isdigit(c))
            {
              return expect_number(c);
            }
            else if(c == '_' or std::isalpha(c))
            {
              return expect_identifier(c);
            }

            return {token::error, line_number_};
          }
        }
      }

      return {token::eof, line_number_};
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
      return result;
    }

    token expect_terminated_string()
    {
      int original_line_number = line_number_;
      std::string lexeme;

      while(peek() != '"' and not is_at_end())
      {
        if(peek() == '\n')
        {
          ++line_number_;
        }

        lexeme.push_back(advance());
      }

      if(is_at_end())
      {
        return {token::error, "Unterminated string", original_line_number};
      }

      // consume terminating quote
      advance();

      // XXX we'd handle escape sequences here
      std::string literal = lexeme;

      return {token::string, lexeme, literal, original_line_number};
    }

    token expect_number(char first_digit)
    {
      std::string lexeme;
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

      return {token::number, lexeme, literal, line_number_};
    }

    token expect_identifier(char first_character)
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

      return {token::classify_identifier(lexeme), lexeme, line_number_};
    }

    std::string_view source_;
    int line_number_;
};

