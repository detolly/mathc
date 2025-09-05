#pragma once

#include <utility>

#include <fixed_string.hpp>
#include <lexer.hpp>
#include <node.hpp>
#include <parser.hpp>
#include <print.hpp>

namespace mathc
{

constexpr static inline node operate(const node& a_node, const node& b_node, operation_type type)
{
    const auto& a = std::get<constant_node>(a_node).value;
    const auto& b = std::get<constant_node>(b_node).value;

    switch(type) {
        case operation_type::mul: return make_node<constant_node>(a * b);
        case operation_type::div: return make_node<constant_node>(a / b);
        case operation_type::add: return make_node<constant_node>(a + b);
        case operation_type::sub: return make_node<constant_node>(a - b);
        case operation_type::exp: return make_node<constant_node>(a ^ b);
    }

    std::unreachable();
}

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

// patterns

template<fixed_string s>
struct ce_any_node
{
    constexpr static auto name() { return s; }
    constexpr static size_t extent() { return 1; }
};

template<fixed_string s>
struct ce_symbol_node
{
    constexpr static auto name() { return s; }
    constexpr static size_t extent() { return 1; }
};

template<fixed_string s>
struct ce_const_var
{
    constexpr static auto name() { return s; }
    constexpr static size_t extent() { return 1; }
};

template<auto n, fixed_string _name>
struct ce_const_value
{
    constexpr static auto name() { return _name; }
    constexpr static auto value() { return n; }
    constexpr static size_t extent() { return 1; }
};

template<auto _left, auto _right, operation_type type, fixed_string _name>
struct ce_op_node
{
    constexpr static auto name() { return _name; }
    constexpr static auto left() { return _left; }
    constexpr static auto right() { return _right; }
    constexpr static auto operation() { return type; }

    constexpr static size_t extent() { return 1 + _right.extent() + _left.extent(); }
};

template<fixed_string _function_name, fixed_string _name, auto... args>
struct ce_func_node
{
    constexpr static auto function_name() { return _function_name; }
    constexpr static auto name() { return _name; }

    constexpr static size_t extent() { return 1 + (args.extent() + ...); }
};

template<size_t extent>
struct pattern_context
{
    using entry = std::tuple<hash_t, hash_t, const node*>;

    constexpr void insert(hash_t name_hash, hash_t node_hash, const node& node)
    {
        if (name_hash == hash(""sv))
            return;

        ctx[index] = { name_hash, node_hash, &node };
        index++;
    }

    constexpr bool exists(hash_t name_hash, hash_t node_hash)
    {
        for (auto i = 0u; i < index; i++) {
            const auto& [_name_hash, _node_hash, node] = ctx[i];
            if (name_hash == _name_hash && node_hash == _node_hash)
                return true;
        }

        return false;
    }

    template<fixed_string str>
    constexpr node& get() const
    {
        constexpr static auto str_hash = hash(str.view());

        for (auto i = 0u; i < index; i++) {
            const auto& [_name_hash, _node_hash, node] = ctx[i];
            if (_name_hash == str_hash)
                return const_cast<mathc::node&>(*node);
        }

        std::unreachable();
    }

    constexpr bool can_be_inserted(const hash_t name_hash, hash_t node_hash)
    {
        for (auto i = 0u; i < index; i++) {
            const auto& [_name_hash, _node_hash, node] = ctx[i];

            // If name exists but not with the same node.
            if (name_hash == _name_hash && node_hash != _node_hash)
                return false;
        }

        return true;
    }

    std::array<entry, extent> ctx;
    size_t index{ 0 };
};

template<typename T>
concept number_t = (std::is_integral_v<T> || std::is_floating_point_v<T>);

template<auto n>
struct pattern_impl
{
    constexpr static auto extent() { return n.extent(); }

    template<fixed_string name = "", typename T> consteval static auto mul(const T&)
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, T{}, operation_type::mul, name>{}>{};
    }

    template<fixed_string name = "", typename T> consteval static auto div(const T&)
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, T{}, operation_type::div, name>{}>{};
    }

    template<fixed_string name = "", typename T> consteval static auto add(const T&)
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, T{}, operation_type::add, name>{}>{};
    }

    template<fixed_string name = "", typename T> consteval static auto sub(const T&)
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, T{}, operation_type::sub, name>{}>{};
    }

    template<fixed_string name = "", typename T> consteval static auto exp(const T&)
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, T{}, operation_type::exp, name>{}>{};
    }

    template<bool should_match_commutative>
    constexpr static inline bool matches(auto& ctx, const node&);
};

struct pattern
{
    template<fixed_string name = "">
    constexpr static auto any() { return pattern_impl<ce_any_node<name>{}>{}; }

    template<fixed_string name = "">
    constexpr static auto var() { return pattern_impl<ce_symbol_node<name>{}>{}; }

    template<fixed_string name = "">
    constexpr static auto cvar() { return pattern_impl<ce_const_var<name>{}>{}; }

    template<auto number, fixed_string s = "">
        requires(number_t<decltype(number)>)
    constexpr static auto constant() { return pattern_impl<ce_const_value<number, s>{}>{}; }

    template<fixed_string function_name, fixed_string name = "", typename... Args>
    constexpr static auto func(const Args&...) { return pattern_impl<ce_func_node<function_name, name, Args{}...>{}>{}; }

    template<operation_type type, fixed_string s = "", typename Left, typename Right>
    constexpr static auto op(const Left&, const Right&) { return pattern_impl<ce_op_node<Left{}, Right{}, type, s>{}>{}; }
};

template<bool should_match_commutative>
struct ce_matcher
{

    template<auto str>
    constexpr static bool check_or_insert_pattern_context(auto& ctx,
                                                          const node& n,
                                                          const auto& node)
    {
        constexpr static auto str_hash = hash(str.view());
        const auto node_hash = node_hasher(node);

        if (ctx.exists(str_hash, node_hash))
            return true;

        if (!ctx.can_be_inserted(str_hash, node_hash))
            return false;

        ctx.insert(str_hash, node_hasher(node), n);
        return true;
    }

    template<auto n, fixed_string name>
    constexpr static bool operator()(auto& ctx,
                                     const node& _node,
                                     const ce_const_value<n, name>&,
                                     const constant_node& constant)
    {
        if (n == constant.value) {
            ctx.insert(hash(name.view()), hash(_node), _node);
            return true;
        }

        return false;
    }

    template<auto left, auto right, operation_type type, fixed_string name>
    constexpr static bool operator()(auto& ctx,
                                     const node& _node,
                                     const ce_op_node<left, right, type, name>&,
                                     const op_node& op)
    {
        if (op.type != type)
            return false;

        auto original_index = ctx.index;
        if (left.template matches<should_match_commutative>(ctx, *op.left) && right.template matches<should_match_commutative>(ctx, *op.right)) {
            ctx.insert(hash(name.view()), hash(_node), _node);
            return true;
        }

        ctx.index = original_index;
        if constexpr (!should_match_commutative || !is_commutative(type))
            return false;

        if (left.template matches<should_match_commutative>(ctx, *op.right) && right.template matches<should_match_commutative>(ctx, *op.left)) {
            ctx.insert(hash(name.view()), hash(_node), _node);
            return true;
        }

        ctx.index = original_index;
        return false;
    }

    template<auto arg, auto... args>
    constexpr static bool check_argument(auto& ctx, const auto& arguments, size_t i = 0u)
    {
        if constexpr (sizeof...(args) > 0)
            return arg.template matches<should_match_commutative>(ctx, arguments[i]) &&
                   check_argument<args...>(ctx, arguments, i + 1);

        return arg.template matches<should_match_commutative>(ctx, arguments[i]);
    }

    template<auto fn_name, auto name, auto... args>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_func_node<fn_name, name, args...>&,
                                     const function_call_node& actual_node)
    {
        if (actual_node.arguments.size() != sizeof...(args))
            return false;

        if (fn_name.view() != actual_node.function_name)
            return false;

        const auto arguments_match = check_argument<args...>(ctx, actual_node.arguments);
        if (!arguments_match)
            return false;

        ctx.insert(hash(name.view()), node_hasher(actual_node), node);
        return true;
    }

    template<auto s>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_const_var<s>&,
                                     const constant_node& actual_node)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node);
    }

    template<auto s>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_symbol_node<s>&,
                                     const symbol_node& actual_node)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node);
    }

    template<auto s>
    constexpr static bool operator()(auto& ctx,
                                     const node& node,
                                     const ce_any_node<s>&,
                                     const auto& actual_node)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node);
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
template<bool should_match_commutative>
constexpr inline bool pattern_impl<ce_node>::matches(auto& ctx, const node& node)
{
    return std::visit([&node, &ctx](const auto& actual_node) {
        return ce_matcher<should_match_commutative>{}(ctx, node, ce_node, actual_node);
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

    pattern_context<T::extent()> ctx;
    const auto& node = node_result.value();
    return T::template matches<true>(ctx, node);
}

#ifndef NO_TEST
static_assert(pattern_test("1", pattern::constant<1>()));
static_assert(pattern_test("1", pattern::cvar()));
static_assert(pattern_test("1+1", pattern::constant<1>().add(pattern::constant<1>())));
static_assert(pattern_test("5*2", pattern::any<"x">().mul(pattern::constant<2>())));
static_assert(pattern_test("5*2", pattern::any<"x">().mul(pattern::constant<5>()))); // commutativity
static_assert(pattern_test("sqrt(2)", pattern::func<"sqrt">(pattern::cvar())));
static_assert(pattern_test("x", pattern::var()));
#endif

}
