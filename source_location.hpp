#pragma once

#include <fmt/format.h>
#include <format>

struct source_location
{
  int line = 1;
  int column = 1;

  void advance_line()
  {
    line++;
    column = 1;
  }

  void advance_column()
  {
    column++;
  }

  bool operator==(const source_location&) const = default;
};

template<>
struct fmt::formatter<source_location>
{
  template<class ParseContext>
  constexpr auto parse(ParseContext& ctx) const
  {
    return ctx.begin();
  }

  template<class FormatContext>
  constexpr auto format(const source_location loc, FormatContext& ctx) const
  {
    return fmt::format_to(ctx.out(), "Line: {}, Column: {}", loc.line, loc.column);
  }
};

template<>
struct std::formatter<source_location>
{
  template<class ParseContext>
  constexpr auto parse(ParseContext& ctx) const
  {
    return ctx.begin();
  }

  template<class FormatContext>
  constexpr auto format(const source_location loc, FormatContext& ctx) const
  {
    return std::format_to(ctx.out(), "Line: {}, Column: {}", loc.line, loc.column);
  }
};

