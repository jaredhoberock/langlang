#pragma once

#include <concepts>
#include <variant>
#include <memory>
#include <type_traits>
#include <utility>

namespace detail
{


template<class T>
class box
{
  public:
    box(const T& value)
      : value_(std::make_unique<T>(value))
    {}

    box(T&& value)
      : value_(std::make_unique<T>(std::move(value)))
    {}

    box(box&&) = default;

    box(const box& other)
      : value_(std::make_unique<T>(other.value()))
    {}

    box& operator=(box&&) = default;

    box& operator=(const box& other)
    {
      value() = other.value();
      return *this;
    }

    T& value()
    {
      return *value_;
    }

    const T& value() const
    {
      return *value_;
    }

  private:
    std::unique_ptr<T> value_;
};


// see https://stackoverflow.com/a/1956217/722294
namespace 
{


template<class T, int discriminator>
struct is_complete {  
  template<class U, int = sizeof(U)>
  static std::true_type test(int);

  template<class>
  static std::false_type test(...);

  static const bool value = decltype(test<T>(0))::value;
};


} // end anonymous namespace

#define IS_COMPLETE(X) is_complete<X,__COUNTER__>::value


template<class T>
using box_if_incomplete_t = std::conditional_t<IS_COMPLETE(T), T, box<T>>;


template<class Function>
struct unbox_and_call
{
  Function f_;

  // forward unboxed arguments along
  template<class Arg>
  static Arg&& unbox_if(Arg&& arg)
  {
    return std::forward<Arg>(arg);
  }

  template<class Arg>
  static Arg& unbox_if(detail::box<Arg>& arg)
  {
    return arg.value();
  }

  template<class Arg>
  static const Arg& unbox_if(const detail::box<Arg>& arg)
  {
    return arg.value();
  }

  template<class Arg>
  static Arg&& unbox_if(detail::box<Arg>&& arg)
  {
    return std::move(arg.value());
  }

  // forward unboxxed arguments to f
  template<class... Args>
  decltype(auto) operator()(Args&&... args) const
  {
    return std::forward<Function>(f_)(unbox_if(std::forward<Args>(args))...);
  }
};


} // end detail


template<class... Types>
class recursive_variant : public std::variant<detail::box_if_incomplete_t<Types>...>
{
  public:
    using super_t = std::variant<detail::box_if_incomplete_t<Types>...>;
    using super_t::super_t;

#if not defined(__circle_lang__) and defined(__clang_major__) and (__clang_major__ < 16)
    // older clang needs this extra ctor
    template<class U>
      requires std::constructible_from<super_t, U&&>
    recursive_variant(U&& value)
      : super_t(std::forward<U>(value))
    {}
#endif // old clang version

    // add a boxing constructor
    template<class U>
      requires (not std::constructible_from<super_t, U&&>
                and std::constructible_from<super_t, detail::box<std::decay_t<U>>>)
    recursive_variant(U&& value)
      : super_t(detail::box<std::decay_t<U>>(std::forward<U>(value)))
    {}
};


namespace detail
{


template<class Variant>
constexpr Variant&& super(Variant&& var)
{
  return std::forward<Variant>(var);
}

template<class... Types>
constexpr typename recursive_variant<Types...>::super_t& super(recursive_variant<Types...>& var)
{
  return var;
}

template<class... Types>
constexpr const typename recursive_variant<Types...>::super_t& super(const recursive_variant<Types...>& var)
{
  return var;
}

template<class... Types>
constexpr typename recursive_variant<Types...>::super_t&& super(recursive_variant<Types...>&& var)
{
  return std::move(var);
}


template<class T>
constexpr std::size_t index_of()
{
  return 1;
}

template<class T, class Head, class... Tail>
constexpr std::size_t index_of()
{
  if constexpr (std::same_as<T,Head>)
  {
    return 0;
  }

  return index_of<T,Tail...>();
}


template<class T>
constexpr std::size_t count_occurrences()
{
  return 0;
}

template<class T, class Head, class... Tail>
constexpr std::size_t count_occurrences()
{
  if constexpr (std::same_as<T,Head>)
  {
    return 1 + count_occurrences<T,Tail...>();
  }

  return count_occurrences<T,Tail...>();
}

template<class T, class... Types>
constexpr bool occurs_exactly_once()
{
  return 1 == count_occurrences<T,Types...>();
}


template<class T>
struct get_visitor
{
  T& operator()(T& arg) const
  {
    return arg;
  }

  const T& operator()(const T& arg) const
  {
    return arg;
  }

  template<class U>
  const T& operator()(const U& arg) const
  {
    throw std::bad_variant_access();
    return *reinterpret_cast<const T*>(&arg);
  }

  template<class U>
  T& operator()(U& arg) const
  {
    throw std::bad_variant_access();
    return *reinterpret_cast<T*>(&arg);
  }
};


} // end detail


template<class Visitor, class Variant, class... Variants>
constexpr decltype(auto) visit(Visitor&& visitor, Variant&& var, Variants&&... vars)
{
  // unbox boxxed types before the visitor sees them
  detail::unbox_and_call<Visitor&&> unboxing_visitor{std::forward<Visitor>(visitor)};

  // unbox recursive_variants into std::variant before passing to std::visit
  return std::visit(unboxing_visitor, detail::super(std::forward<Variant>(var)), detail::super(std::forward<Variants>(vars))...);
}


template<class T, class... Types>
constexpr bool holds_alternative(const recursive_variant<Types...>& var)
{
  static_assert(detail::occurs_exactly_once<T,Types...>(), "T must occur exactly once in alternatives");

  return detail::index_of<T,Types...>() == var.index();
}


template<class T, class... Types>
constexpr T& get(recursive_variant<Types...>& var)
{
  static_assert(detail::occurs_exactly_once<T,Types...>(), "T must occur exactly once in alternatives");

  return visit(detail::get_visitor<T>{}, var);
}

template<class T, class... Types>
constexpr const T& get(const recursive_variant<Types...>& var)
{
  static_assert(detail::occurs_exactly_once<T,Types...>(), "T must occur exactly once in alternatives");

  return visit(detail::get_visitor<T>{}, var);
}


// clean up after ourself
#undef IS_COMPLETE

