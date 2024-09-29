#pragma once

#include "syntax.hpp"
#include "token_range.hpp"
#include <concepts>
#include <expected>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>

namespace parsers
{
namespace detail
{


template<class T, std::size_t N>
concept has_tuple_element =
  requires(T t) {
    typename std::tuple_element_t<N, std::remove_const_t<T>>;
    { get<N>(t) } -> std::convertible_to<const std::tuple_element_t<N, T>&>;
  };

template<class T>
concept tuple_like = !std::is_reference_v<T> 
  && requires(T t) { 
    typename std::tuple_size<T>::type; 
    requires std::derived_from<
      std::tuple_size<T>, 
      std::integral_constant<std::size_t, std::tuple_size_v<T>>
    >;
  } && []<std::size_t... N>(std::index_sequence<N...>) { 
    return (has_tuple_element<T, N> && ...); 
  }(std::make_index_sequence<std::tuple_size_v<T>>());


template<class V, std::size_t N>
concept has_variant_alternative =
  requires(V v) {
    typename std::variant_alternative_t<N, std::remove_const_t<V>>;
    { get<N>(v) } -> std::convertible_to<const std::variant_alternative_t<N,V>&>;
  };

template<class V>
concept variant_like = !std::is_reference_v<V>
  && requires(V v) {
    typename std::variant_size<V>::type;
    requires std::derived_from<
      std::variant_size<V>,
      std::integral_constant<std::size_t, std::variant_size_v<V>>
    >;
  } && []<std::size_t... N>(std::index_sequence<N...>) {
    return (has_variant_alternative<V, N> && ...);
  }(std::make_index_sequence<std::variant_size_v<V>>());


} // end detail


template<class T>
concept tuple_like = detail::tuple_like<std::remove_cvref_t<T>>;

template<class V>
concept variant_like = detail::variant_like<std::remove_cvref_t<V>>;



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
  std::span<token> remaining;
};

template<class S>
using success_value_t = decltype(S::value);

struct failure
{
  std::string message;
  std::span<token> remaining;
};


template<class P>
concept parser = std::invocable<P,std::span<token>>;

template<class P, class T>
concept parser_for =
  parser<P>
  and requires(P p, std::span<token> tokens)
  {
    { p(tokens) } -> std::same_as<std::expected<success<T>,failure>>;
  }
;



template<class E>
using expected_value_t = typename E::value_type;


template<parser P>
using parser_success_t = success_value_t<expected_value_t<decltype(std::declval<P>()(std::declval<std::span<token>>()))>>; 


// transform the result of a successful parse with f
template<parser P, std::invocable<parser_success_t<P>> F>
constexpr parser auto transform(P p, F f)
{
  using R = std::invoke_result_t<F,parser_success_t<P>>; 

  return [=](std::span<token> tokens) -> std::expected<success<R>,failure>
  {
    auto result = p(tokens);
    if(result)
    {
      return success{f(result->value), result->remaining};
    }

    return std::unexpected(result.error());
  };
}


// shorthand for transform
template<parser P, std::invocable<parser_success_t<P>> F>
constexpr parser auto operator>>(P p, F f)
{
  return transform(p, f);
}


template<class R>
constexpr inline auto to = []<class T>(T&& value)
{
  if constexpr (std::constructible_from<R,T&&>)
  {
    return R{std::forward<T>(value)};
  }
  else if constexpr (tuple_like<T&&>)
  {
    return std::apply([]<class... Es>(Es&&... ctor_args)
    {
      return R{std::forward<Es>(ctor_args)...};
    }, std::forward<T>(value));
  }
  else if constexpr (variant_like<T&&>)
  {
    return std::visit([]<class A>(A&& alt)
    {
      return R{std::forward<A>(alt)};
    }, std::forward<T>(value));
  }
};


// transform the result of a failed parse with f
template<parser P, std::invocable<std::string&&> F>
constexpr parser auto transform_error(P p, F f)
{
  using R = parser_success_t<P>;

  return [=](std::span<token> tokens) -> std::expected<success<R>,failure>
  {
    auto result = p(tokens);
    if(result)
    {
      return success{std::move(result->value), result->remaining};
    }

    failure fail{f(std::move(result.error().message)), result.error().remaining};

    return std::unexpected(std::move(fail));
  };
}


template<class R, parser A, parser B>
  requires (std::constructible_from<R,parser_success_t<A>> and std::constructible_from<R,parser_success_t<B>>)
constexpr parser_for<R> auto choice_r(A a, B b)
{
  return [=](std::span<token> tokens) -> std::expected<success<R>,failure>
  {
    auto a_result = a(tokens);
    if(a_result) return a_result;

    auto b_result = b(tokens);
    if(b_result) return b_result;

    if(b_result.error().remaining.size() < a_result.error().remaining.size())
    {
      // b parsed further than a
      return std::unexpected(b_result.error());
    }

    return std::unexpected(a_result.error());
  };
}


template<parser A, parser B>
struct choice_parser
{
  using value_type = std::variant<parser_success_t<A>,parser_success_t<B>>;

  constexpr std::expected<success<value_type>,failure> operator()(std::span<token> tokens) const
  {
    auto a_result = (a >> to<value_type>)(tokens);
    if(a_result) return a_result;

    auto b_result = (b >> to<value_type>)(tokens);
    if(b_result) return b_result;

    if(b_result.error().remaining.size() < a_result.error().remaining.size())
    {
      // b parsed further than a
      return std::unexpected(b_result.error());
    }

    return std::unexpected(a_result.error());
  }

  template<parser C, parser D>
  constexpr auto operator|(choice_parser<C,D> other) const
  {
    using value_type = std::variant<
      parser_success_t<A>,
      parser_success_t<B>,
      parser_success_t<C>,
      parser_success_t<D>
    >;

    auto self = *this;

    return [=](std::span<token> tokens) -> std::expected<success<value_type>,failure>
    {
      auto self_result = (self >> to<value_type>)(tokens);
      if(self_result) return self_result;

      auto other_result = (other >> to<value_type>)(tokens);
      if(other_result) return other_result;

      if(other_result.error().remaining.size() < self_result.error().remaining.size())
      {
        // other parsed further than self
        return std::unexpected(other_result.error());
      }

      return std::unexpected(self_result.error());
    };
  }

  template<parser O>
  constexpr auto operator|(O other) const
  {
    using value_type = std::variant<parser_success_t<A>,parser_success_t<B>,parser_success_t<O>>;
    auto self = *this;

    return [=](std::span<token> tokens) -> std::expected<success<value_type>,failure>
    {
      auto self_result = (self >> to<value_type>)(tokens);
      if(self_result) return self_result;

      auto other_result = (other >> to<value_type>)(tokens);
      if(other_result) return self_result;

      if(other_result.error().remaining.size() < self_result.error().remaining.size())
      {
        // other parsed further than self
        return std::unexpected(other_result.error());
      }

      return std::unexpected(self_result.error());
    };
  }

  A a;
  B b;
};


template<parser A, parser B>
  requires (not std::same_as<parser_success_t<A>,parser_success_t<B>>)
constexpr choice_parser<A,B> choice(A a, B b)
{
  return {a,b};
}


template<parser A, parser B>
constexpr parser auto operator|(A a, B b)
{
  if constexpr (std::same_as<parser_success_t<A>,parser_success_t<B>>)
  {
    using R = parser_success_t<A>;
    return choice_r<R>(a,b);
  }
  else
  {
    return choice(a,b);
  }
}


template<parser A, parser B>
struct sequence_parser
{
  A a;
  B b;

  using value_type = std::tuple<parser_success_t<A>,parser_success_t<B>>;

  constexpr std::expected<success<value_type>,failure> operator()(std::span<token> tokens) const
  {
    auto a_result = a(tokens);
    if(not a_result) return std::unexpected(a_result.error());

    auto xfrm_b = transform_error(b, [&](std::string&& message)
    {
      // XXX we need a convenient way to communicate the parsing ctx downstream to b
      return std::format("{} after {}", message, typeid(*a_result).name());
    });

    auto b_result = xfrm_b(a_result->remaining);

    if(not b_result) return std::unexpected(b_result.error());

    return success{std::tuple(a_result->value, b_result->value), b_result->remaining};
  }

  template<parser C, parser D>
  constexpr parser auto operator+(sequence_parser<C,D> other) const
  {
    using value_type = std::tuple<
      parser_success_t<A>,
      parser_success_t<B>,
      parser_success_t<C>,
      parser_success_t<D>
    >;

    auto self = *this;

    return [=](std::span<token> tokens) -> std::expected<success<value_type>,failure>
    {
      auto self_result = self(tokens);
      if(not self_result) return std::unexpected(self_result.error());

      auto other_result = other(self_result->remaining);
      if(not other_result) return std::unexpected(other_result.error());

      auto [a, b] = std::move(self_result->value);
      auto [c, d] = std::move(other_result->value);

      return success{std::tuple(std::move(a), std::move(b), std::move(c), std::move(d)), other_result->remaining};
    };
  };

  template<parser O>
  constexpr parser auto operator+(O other) const
  {
    using value_type = std::tuple<parser_success_t<A>,parser_success_t<B>,parser_success_t<O>>;
    auto self = *this;

    return [=](std::span<token> tokens) -> std::expected<success<value_type>,failure>
    {
      auto self_result = self(tokens);
      if(not self_result) return std::unexpected(self_result.error());

      auto other_result = other(self_result->remaining);
      if(not other_result) return std::unexpected(other_result.error());

      auto [a, b] = std::move(self_result->value);

      return success{std::tuple(std::move(a), std::move(b), std::move(other_result->value)), other_result->remaining};
    };
  }
};


template<parser A, parser B>
constexpr sequence_parser<A,B> sequence(A a, B b)
{
  return {a,b};
}

template<parser A, parser B>
constexpr sequence_parser<A,B> operator+(A a, B b)
{
  return sequence(a,b);
}


template<std::size_t I, parser P>
constexpr parser auto get(P p)
{
  return transform(p, []<class T>(T&& tuple)
  {
    return std::get<I>(std::forward<T>(tuple));
  });
}


// returns a parser which parses a token of the given kind
constexpr parser_for<::token> auto token(::token::kind k)
{
  return [k](std::span<::token> tokens) -> std::expected<success<::token>,failure>
  {
    if(peek(tokens).which_kind() != k)
    {
      failure result{std::format("Expected '{}'", ::token::to_string(k)), tokens};
      return std::unexpected(result);
    }

    return success{advance(tokens), tokens};
  };
}


constexpr parser auto operator|(char a, parser auto b)
{
  return token(::token::to_kind(a)) | b;
}

constexpr parser auto operator|(parser auto a, char b)
{
  return a | token(::token::to_kind(b));
}

constexpr parser auto operator+(char a, parser auto b)
{
  return token(::token::to_kind(a)) + b;
}

constexpr parser auto operator+(parser auto a, char b)
{
  return a + token(::token::to_kind(b));
}


constexpr parser_for<literal> auto number = transform(token(::token::number), [](::token n)
{
  return literal{n.number_literal()};
});

constexpr parser_for<literal> auto string = transform(token(::token::string), [](::token s)
{
  return literal{s.string_literal()};
});

constexpr parser_for<literal> auto true_ = transform(token(::token::true_), [](auto)
{
  return literal{true};
});

constexpr parser_for<literal> auto false_ = transform(token(::token::false_), [](auto)
{
  return literal{false};
});

constexpr parser_for<literal> auto nil = transform(token(::token::nil), [](auto)
{
  return literal{nullptr};
});

constexpr parser_for<::literal> auto literal = number | string | true_ | false_ | nil;

constexpr parser_for<::variable> auto variable = token(::token::identifier) >> to<::variable>;


} // end parsers

