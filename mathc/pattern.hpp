#pragma once

#include <fixed_string.hpp>
#include <lexer.hpp>
#include <node.hpp>
#include <parser.hpp>
#include <print.hpp>

namespace mathc
{

constexpr static inline bool is_commutative(operation_type type)
{
    switch(type) {
        case mathc::operation_type::add:
        case mathc::operation_type::mul:
            return true;
        case mathc::operation_type::sub:
        case mathc::operation_type::exp:
        case mathc::operation_type::div:
            return false;
    }
}

enum struct match_commutative
{
    yes,
    no
};

// pattern nodes

template<std::size_t _extent>
struct extent_t
{
    consteval static std::size_t extent() { return _extent;}
};

template<std::size_t s>
struct hierarchy_placement_t
{
    consteval static std::size_t hierarchy_placement() { return s; }
};

template<fixed_string _name>
struct name_t
{
    consteval static auto name() { return _name; }
};

template<fixed_string s, std::size_t hp>
struct trivial_findable_t
{
    template<fixed_string n>
    consteval static ssize_t find() { return n.view() == s.view() ? static_cast<ssize_t>(hp) : -1; }
};

template<fixed_string name, std::size_t hp>
struct ce_any_node : trivial_findable_t<name, hp>, hierarchy_placement_t<hp>, extent_t<1>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_any_node<name, hp + n>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<fixed_string name, std::size_t hp, typename... Ts>
    requires((node_type<Ts> && ...))
struct ce_any_of_node : trivial_findable_t<name, hp>, extent_t<1>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_any_of_node<name, hp + n, Ts...>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<fixed_string name, std::size_t hp, typename... Ts>
    requires((node_type<Ts> && ...))
struct ce_none_of_node : trivial_findable_t<name, hp>, extent_t<1>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_none_of_node<name, hp + n, Ts...>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<fixed_string name, std::size_t hp>
struct ce_symbol_node : trivial_findable_t<name, hp>, extent_t<1>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_symbol_node<name, hp + n>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<fixed_string name, std::size_t hp>
struct ce_const_var : trivial_findable_t<name, hp>, extent_t<1>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_const_var<name, hp + n>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<auto val, fixed_string name, std::size_t hp>
struct ce_const_value : trivial_findable_t<name, hp>, extent_t<1>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n> using increment_t = ce_const_value<val, name, hp + n>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }
};

template<auto left, auto right, operation_type type, fixed_string name, match_commutative mc, std::size_t hp>
struct ce_op_node : extent_t<1 + right.extent() + left.extent()>, hierarchy_placement_t<hp>, name_t<name>
{
    template<std::size_t n>
    using increment_t = ce_op_node<left.template increment<n>(), right.template increment<n>(), type, name, mc, hp + n>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }

    template<fixed_string str>
    consteval static ssize_t find()
    {
        if constexpr (name.view() == str.view())
            return static_cast<ssize_t>(hp);

        constexpr static auto first_result = left.template find<str>();
        if constexpr (first_result >= 0)
            return first_result;

        constexpr static auto second_result = right.template find<str>();
        if constexpr (second_result >= 0)
            return second_result;

        return -1;
    }
};

template<fixed_string function_name, fixed_string name, std::size_t hp, auto... args>
struct ce_func_node : extent_t<1 + (args.extent() + ...)>, name_t<name>, hierarchy_placement_t<hp>
 {
    template<std::size_t n>
    using increment_t = ce_func_node<function_name, name, hp + n, (args.template increment<n>(), ...)>;
    template<std::size_t n> consteval static auto increment() { return increment_t<n>{}; }

    template<fixed_string str, auto... _args>
        requires (sizeof...(_args) == 0)
    consteval static ssize_t find_iter()
    {
        return -1;
    }

    template<fixed_string str, auto _arg, auto... _args>
    consteval static ssize_t find_iter()
    {
        if constexpr (str.view() == _arg.name().view())
            return static_cast<ssize_t>(_arg.hierarchy_placement());

        return find_iter<str, _args...>();
    }

    template<fixed_string str>
    consteval static ssize_t find()
    {
        if constexpr (name.view() == str.view())
            return static_cast<ssize_t>(hp);

        return find_iter<str, args...>();
    }
};

template<auto n>
struct pattern_impl;

template<pattern_impl pattern>
struct pattern_context
{
    using entry = std::tuple<bool, hash_t, const node*>;

    template<fixed_string str>
    consteval static std::size_t get_index()
    {
        constexpr static auto result = pattern.template find<str>();
        if constexpr (result == -1)
            static_assert(false, "Name was not found");

        return static_cast<std::size_t>(result);
    }

    template<std::size_t hierarchy_placement>
    constexpr void insert(const hash_t node_hash, const node& node)
    {
        ctx[hierarchy_placement] = { true, node_hash, &node };
    }

    template<std::size_t hierarchy_placement>
    constexpr bool exists_at_index()
    {
        return std::get<0>(ctx[hierarchy_placement]);
    }

    template<std::size_t hierarchy_placement>
    constexpr bool same_at_index(const hash_t node_hash)
    {
        const auto& [exists, _node_hash, node] = ctx[hierarchy_placement];
        return node_hash == _node_hash;
    }

    template<fixed_string str>
    constexpr node& get() const
    {
        const auto& [_, _, node] = ctx[get_index<str>()];
        return const_cast<mathc::node&>(*node);
    }

    std::array<entry, pattern.extent()> ctx;
};

template<typename T>
concept number_type = (std::is_integral_v<T> || std::is_floating_point_v<T>);

template<auto pattern_node>
struct pattern_impl
{
    template<std::size_t n>
    consteval static auto increment() { return pattern_impl<pattern_node.template increment<n>()>{}; }

    template<fixed_string str>
    consteval static auto find() { return pattern_node.template find<str>(); }

    consteval static auto hierarchy_placement() { return pattern_node.hierarchy_placement(); }
    consteval static auto extent() { return pattern_node.extent(); }
    consteval static auto name() { return pattern_node.name(); }

    #define f(type) \
    template<fixed_string name = "", match_commutative mc = match_commutative::yes, typename T> \
    consteval static auto type(const T&) \
    { \
        return pattern_impl<ce_op_node<pattern_impl<pattern_node.template increment<1>()>{}, T::template increment<pattern_node.extent() + 1>(), operation_type::type, name, mc, pattern_node.hierarchy_placement()>{}>{}; \
    }

    f(mul)
    f(div)
    f(add)
    f(sub)
    f(exp)

    #undef f

    constexpr static inline bool matches(auto& ctx, const node&);
};

struct pattern
{
    template<fixed_string name = "">
    constexpr static auto any() { return pattern_impl<ce_any_node<name, 0>{}>{}; }

    template<fixed_string name = "", typename... Ts>
    constexpr static auto any_of() { return pattern_impl<ce_any_of_node<name, 0, Ts...>{}>{}; }

    template<fixed_string name = "", typename... Ts>
    constexpr static auto none_of() { return pattern_impl<ce_none_of_node<name, 0, Ts...>{}>{}; }

    template<fixed_string name = "">
    constexpr static auto var() { return pattern_impl<ce_symbol_node<name, 0>{}>{}; }

    template<fixed_string name = "">
    constexpr static auto cvar() { return pattern_impl<ce_const_var<name, 0>{}>{}; }

    template<auto number, fixed_string s = "">
        requires(number_type<decltype(number)>)
    constexpr static auto constant() { return pattern_impl<ce_const_value<number, s, 0>{}>{}; }

    template<std::size_t I, typename... Ts>
    struct offset
    {
        constexpr static std::size_t value = 1 + offset<I-1, Ts...>::value + std::tuple_element_t<I-1, std::tuple<Ts...>>::extent();
    };

    template<typename... Ts>
    struct offset<0, Ts...>
    {
        constexpr static std::size_t value = 1;
    };

    template<fixed_string function_name, fixed_string name = "", typename... Args>
    constexpr static auto func(const Args&...) {
        return []<std::size_t... Is>(std::index_sequence<Is...>) {
            return pattern_impl<ce_func_node<function_name, name, 0,
                   Args::template increment<offset<Is, Args...>::value>()...>{}>{};
        }(std::index_sequence_for<Args...>{});
    }

    template<operation_type type, fixed_string s = "", match_commutative mc = match_commutative::yes, typename Left, typename Right>
    constexpr static auto op(const Left&, const Right&) { return pattern_impl<ce_op_node<Left::template increment<1>(), Right::template increment<Left::extent() + 1>(), type, s, mc, 0>{}>{}; }
};

struct ce_matcher
{
    template<fixed_string str, std::size_t hp>
    constexpr static bool check_or_insert_pattern_context(auto& ctx,
                                                          const node& n,
                                                          const auto& node)
    {
        constexpr static auto index_to_use = str.view() == ""sv ? hp : ctx.template get_index<str>();

        const auto node_hash = node_hasher(node);
        if (ctx.template same_at_index<index_to_use>(node_hash))
            return true;

        if (ctx.template exists_at_index<index_to_use>())
            return false;

        ctx.template insert<index_to_use>(node_hasher(node), n);
        return true;
    }

    template<auto n, fixed_string name, std::size_t hp>
    constexpr static bool operator()(auto& ctx,
                                     const node& _node,
                                     const ce_const_value<n, name, hp>&,
                                     const constant_node& constant)
    {
        constexpr static auto index_to_use = name.view() == ""sv ? hp : ctx.template get_index<name>();
        if (n == constant.value) {
            ctx.template insert<index_to_use>(hash(_node), _node);
            return true;
        }

        return false;
    }

    template<auto left, auto right, operation_type type, fixed_string name, match_commutative mc, std::size_t hp>
    constexpr static bool operator()(auto& ctx,
                                     const node& _node,
                                     const ce_op_node<left, right, type, name, mc, hp>&,
                                     const op_node& op)
    {
        constexpr static auto index_to_use = name.view() == ""sv ? hp : ctx.template get_index<name>();
        if (op.type != type)
            return false;

        const auto ctx_backup = auto{ ctx };
        if (left.matches(ctx, *op.left) && right.matches(ctx, *op.right)) {
            ctx.template insert<index_to_use>(hash(_node), _node);
            return true;
        }

        ctx = ctx_backup;
        if constexpr (mc != match_commutative::yes || !is_commutative(type))
            return false;

        if (left.matches(ctx, *op.right) && right.matches(ctx, *op.left)) {
            ctx.template insert<index_to_use>(hash(_node), _node);
            return true;
        }

        ctx = ctx_backup;
        return false;
    }

    template<auto arg, auto... args>
    constexpr static bool check_argument(auto& ctx, const auto& arguments, std::size_t i = 0u)
    {
        if constexpr (sizeof...(args) > 0)
            return arg.matches(ctx, arguments[i]) &&
                   check_argument<args...>(ctx, arguments, i + 1);

        return arg.matches(ctx, arguments[i]);
    }

    template<auto fn_name, auto name, std::size_t hp, auto... args>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_func_node<fn_name, name, hp, args...>&,
                                     const function_call_node& actual_node)
    {
        constexpr static auto index_to_use = name.view() == ""sv ? hp : ctx.template get_index<name>();

        if (actual_node.arguments.size() != sizeof...(args))
            return false;

        if (fn_name.view() != actual_node.function_name)
            return false;

        const auto arguments_match = check_argument<args...>(ctx, actual_node.arguments);
        if (!arguments_match)
            return false;

        ctx.template insert<index_to_use>(node_hasher(actual_node), node);
        return true;
    }

    template<fixed_string s, std::size_t hp>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_const_var<s, hp>&,
                                     const constant_node& actual_node)
    {
        return check_or_insert_pattern_context<s, hp>(ctx, node, actual_node);
    }

    template<fixed_string s, std::size_t hp, typename T, typename... Ts>
        requires(std::is_same_v<T, Ts> || ...)
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_any_of_node<s, hp, Ts...>&,
                                     const T& actual_node)
    {
        return check_or_insert_pattern_context<s, hp>(ctx, node, actual_node);
    }

    template<fixed_string s, std::size_t hp, typename T, typename... Ts>
        requires((!std::is_same_v<T, Ts>) && ...)
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_none_of_node<s, hp, Ts...>&,
                                     const T& actual_node)
    {
        return check_or_insert_pattern_context<s, hp>(ctx, node, actual_node);
    }

    template<fixed_string s, std::size_t hp>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_symbol_node<s, hp>&,
                                     const symbol_node& actual_node)
    {
        return check_or_insert_pattern_context<s, hp>(ctx, node, actual_node);
    }

    template<fixed_string s, std::size_t hp>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_any_node<s, hp>&,
                                     const auto& actual_node)
    {
        return check_or_insert_pattern_context<s, hp>(ctx, node, actual_node);
    }

    constexpr static bool operator()(auto&,
                                     const node&,
                                     const auto&,
                                     const auto&)
    {
        return false;
    }

};

template<auto ce_node>
constexpr inline bool pattern_impl<ce_node>::matches(auto& ctx, const node& node)
{
    return std::visit([&node, &ctx](const auto& actual_node) {
        return ce_matcher::operator()(ctx, node, ce_node, actual_node);
    }, node);
}

// test

template<typename T>
constexpr static inline bool pattern_test(const std::string_view source, const T&)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_result = parser::parse(vec);
    assert(node_result.has_value());

    pattern_context<T{}> ctx;
    const auto& node = node_result.value();
    return T::matches(ctx, node);
}

#ifndef NO_TEST
static_assert(pattern_test("1", pattern::constant<1>()));
static_assert(pattern_test("1", pattern::cvar()));
static_assert(pattern_test("1+1", pattern::constant<1>().add(pattern::constant<1>())));
static_assert(pattern_test("5*2", pattern::any<"x">().mul(pattern::constant<2>())));
static_assert(pattern_test("5*2", pattern::any<"x">().mul(pattern::constant<5>()))); // commutativity
// static_assert(pattern_test("sqrt(2)", pattern::func<"sqrt">(pattern::cvar())));
static_assert(pattern_test("x", pattern::var()));
#endif

}
