#pragma once

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>
#include <variant>

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
      : box(other.value())
    {}

    box& operator=(box&&) = default;

    box& operator=(const box& other)
    {
      value() = other.value();
      return *this;
    }

    T& value() &
    {
      return *value_;
    }

    const T& value() const &
    {
      return *value_;
    }

    T&& value() &&
    {
      return std::move(*value_);
    }

  private:
    std::unique_ptr<T> value_;
};


template<class T>
struct is_box : std::false_type {};

template<class T>
struct is_box<box<T>> : std::true_type {};


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

#define IS_COMPLETE(X) detail::is_complete<X,__COUNTER__>::value


template<class T>
using box_if_incomplete_t = std::conditional_t<IS_COMPLETE(T), T, box<T>>;


template<class Function>
struct unbox_and_call
{
  Function f_;

  template<class Arg>
  static decltype(auto) unbox(Arg&& arg)
  {
    using T = std::remove_cvref_t<Arg>;

    if constexpr (is_box<T>::value)
    {
      return std::forward<Arg>(arg).value();
    }
    else
    {
      // arg isn't a box, just forward it
      return std::forward<Arg>(arg);
    }
  }

  // forward unboxxed arguments to f
  template<class... Args>
  decltype(auto) operator()(Args&&... args) const
  {
    return std::forward<Function>(f_)(unbox(std::forward<Args>(args))...);
  }
};


template<class T>
constexpr bool is_one_of_impl()
{
  return false;
}

template<class T, class Type1, class... Types>
constexpr bool is_one_of_impl()
{
  if constexpr (std::same_as<T,Type1>)
  {
    return true;
  }
  else
  {
    return is_one_of_impl<T,Types...>();
  }
}


template<class T, class... Types>
concept is_one_of = is_one_of_impl<T,Types...>();


} // end detail


template<class... Types>
class recursive_variant : public std::variant<detail::box_if_incomplete_t<Types>...>
{
  public:
    using super_t = std::variant<detail::box_if_incomplete_t<Types>...>;
    using super_t::super_t;

    // add a boxing constructor for types that need to be boxed
    template<class U>
      requires (detail::is_one_of<detail::box<std::remove_cvref_t<U>>,Types...> and
                std::constructible_from<U,U&&>)
    recursive_variant(U&& value)
      : super_t(detail::box<std::remove_cvref_t<U>>(std::forward<U>(value)))
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
  return -1;
}

template<class T, class Head, class... Tail>
constexpr std::size_t index_of()
{
  if constexpr (std::same_as<T,Head>)
  {
    return 0;
  }
  else
  {
    return 1 + index_of<T,Tail...>();
  }
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

