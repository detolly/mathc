#pragma once

#include <fixed_string.hpp>
#include <lexer.hpp>
#include <node.hpp>
#include <parser.hpp>

namespace mathc
{

constexpr static inline node operate(const number& a, const number& b, operation_type type)
{
    switch(type) {
        case operation_type::mul: return make_node<constant_node>(a * b);
        case operation_type::div: return make_node<constant_node>(a / b);
        case operation_type::add: return make_node<constant_node>(a + b);
        case operation_type::sub: return make_node<constant_node>(a - b);
        case operation_type::exp: return make_node<constant_node>(a ^ b);
    }
}

constexpr static inline bool is_associative(operation_type type)
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
};

template<fixed_string s>
struct ce_const_var
{
    constexpr static auto name() { return s; }
};

template<auto n>
struct ce_const_value
{
    constexpr static auto value() { return n; }
};

template<auto _left, auto _right, operation_type type>
struct ce_op_node
{
    constexpr static auto left() { return _left; }
    constexpr static auto right() { return _right; }
    constexpr static auto operation() { return type; }
};

struct pattern_context
{
    constexpr void insert(const std::string_view str, hash_t _hash)
    {
        ctx.push_back(std::make_pair(hash(str), _hash));
    }

    constexpr bool exists(hash_t name_hash, hash_t node_hash)
    {
        for(const auto& [_name_hash, _node_hash] : ctx)
            if (name_hash == _name_hash && node_hash == _node_hash)
                return true;

        return false;
    }

    constexpr bool can_be_inserted(const hash_t name_hash, hash_t node_hash)
    {
        for(const auto& [_name_hash, _node_hash] : ctx)
            // If node exists but not with the same name, or name exists but not with the same node.
            if ((name_hash == _name_hash && node_hash != _node_hash) ||
                (name_hash != _name_hash && node_hash == _node_hash))
                return false;

        return true;
    }

    std::vector<std::pair<hash_t, hash_t>> ctx;
};

template<typename T>
concept number_t = (std::is_integral_v<T> || std::is_floating_point_v<T>);

template<auto n>
struct pattern_impl
{
    template<auto right> consteval static auto mul()
        requires(number_t<decltype(right)>)
    {
        return ce_op_node<n, pattern_impl<ce_const_value<right>{}>{}, operation_type::mul>{};
    }

    template<auto right> consteval static auto div()
        requires(number_t<decltype(right)>)
    {
        return ce_op_node<n, pattern_impl<ce_const_value<right>{}>{}, operation_type::div>{};
    }

    template<auto right> consteval static auto add()
        requires(number_t<decltype(right)>)
    {
        return ce_op_node<n, pattern_impl<ce_const_value<right>{}>{}, operation_type::add>{};
    }

    template<auto right> consteval static auto sub()
        requires(number_t<decltype(right)>)
    {
        return ce_op_node<n, pattern_impl<ce_const_value<right>{}>{}, operation_type::sub>{};
    }

    template<auto right> consteval static auto pow()
        requires(number_t<decltype(right)>)
    { 
        return ce_op_node<n, pattern_impl<ce_const_value<right>{}>{}, operation_type::exp>{};
    }

    template<auto right> consteval static auto mul() { return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::mul>{}>{}; }
    template<auto right> consteval static auto div() { return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::div>{}>{}; }
    template<auto right> consteval static auto add() { return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::add>{}>{}; }
    template<auto right> consteval static auto sub() { return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::sub>{}>{}; }
    template<auto right> consteval static auto pow() { return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::exp>{}>{}; }

    constexpr static inline bool matches(const node&);
    constexpr static inline bool matches(pattern_context&, const node&);
};

struct pattern
{
    template<fixed_string s>
    constexpr static auto var() { return pattern_impl<ce_any_node<s>{}>{}; }

    template<fixed_string s>
    constexpr static auto cvar() { return pattern_impl<ce_const_var<s>{}>{}; }

    template<auto s>
        requires(number_t<decltype(s)>)
    constexpr static auto constant() { return pattern_impl<ce_const_value<s>{}>{}; }
};

constexpr static struct
{
    template<auto n>
    constexpr static bool operator()(pattern_context&,
                                     const ce_const_value<n>&,
                                     const constant_node& constant)
    {
        return n == constant.value;
    }

    template<auto left, auto right, operation_type type>
    constexpr static bool operator()(pattern_context& ctx,
                                     const ce_op_node<left, right, type>&,
                                     const op_node& op)
    {
        if (op.type != type)
            return false;

        pattern_context ctx2 = auto{ ctx };
        const auto normal_match = left.matches(ctx, *op.left) && right.matches(ctx, *op.right);
        if (is_commutative(type)) {
            return normal_match || (left.matches(ctx2, *op.right) && right.matches(ctx2, *op.left));
        }

        return normal_match;
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx, const ce_const_var<s>&, const constant_node& node)
    {
        constexpr static auto str_hash = hash(s.view());
        const auto node_hash = node_hasher(node);

        if (ctx.exists(str_hash, node_hash))
            return true;

        if (!ctx.can_be_inserted(str_hash, node_hash))
            return false;

        ctx.insert(s.view(), node_hasher(node));
        return true;
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx, const ce_const_var<s>&, const symbol_node& node)
    {
        constexpr static auto str_hash = hash(s.view());
        const auto node_hash = node_hasher(node);

        if (ctx.exists(str_hash, node_hash))
            return true;

        if (!ctx.can_be_inserted(str_hash, node_hash))
            return false;

        ctx.insert(s.view(), node_hasher(node));
        return true;
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx, const ce_any_node<s>&, const auto& node)
    {
        constexpr static auto str_hash = hash(s.view());
        const auto node_hash = node_hasher(node);

        if (ctx.exists(str_hash, node_hash))
            return true;

        if (!ctx.can_be_inserted(str_hash, node_hash))
            return false;

        ctx.insert(s.view(), node_hasher(node));
        return true;
    }

    constexpr static bool operator()(pattern_context&, const auto&, const auto&) { return false; }

} ce_matcher;

template<auto ce_node>
constexpr bool pattern_impl<ce_node>::matches(pattern_context& ctx, const node& node)
{
    constexpr static auto ce_node_var = ce_node;
    return std::visit([&ctx](const auto& _n){ return ce_matcher(ctx, ce_node_var, _n); }, node);
}

template<auto ce_node>
constexpr inline bool pattern_impl<ce_node>::matches(const node& node)
{
    pattern_context ctx;
    return matches(ctx, node);
}

template<typename T>
constexpr static inline bool pattern_test(const std::string_view source, const T&)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_result = parser::parse(vec);
    assert(node_result.has_value());

    const auto& node = node_result.value();
    return T::matches(node);
}

#ifndef NO_TEST
static_assert(pattern_test("1", pattern::constant<1>()));
static_assert(pattern_test("1+1", pattern::constant<1>().add<pattern::constant<1>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<2>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<5>()>())); // commutativity
#endif

// strategies

struct strategy
{
    constexpr static void rewrite(const node&);
};

constexpr static auto strategies = std::initializer_list<strategy>
{
};

}
