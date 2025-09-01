#pragma once

#include <utility>

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
    using entry = std::tuple<hash_t, hash_t, node&>;

    constexpr void insert(hash_t name_hash, hash_t node_hash, const node& node)
    {
        ctx.emplace_back(name_hash, node_hash, const_cast<mathc::node&>(node));
    }

    constexpr bool exists(hash_t name_hash, hash_t node_hash)
    {
        for(const auto& [_name_hash, _node_hash, _] : ctx)
            if (name_hash == _name_hash && node_hash == _node_hash)
                return true;

        return false;
    }

    template<fixed_string str>
    constexpr node&& get() const
    {
        constexpr static auto str_hash = hash(str.view());
        for(const auto& [_name_hash, _node_hash, node] : ctx)
            if (_name_hash == str_hash)
                return std::move(node);

        std::unreachable();
    }

    constexpr bool can_be_inserted(const hash_t name_hash, hash_t node_hash)
    {
        for(const auto& [_name_hash, _node_hash, _] : ctx)
            // If node exists but not with the same name, or name exists but not with the same node.
            if ((name_hash == _name_hash && node_hash != _node_hash) ||
                (name_hash != _name_hash && node_hash == _node_hash))
                return false;

        return true;
    }

    std::vector<entry> ctx;
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

    constexpr static inline bool matches(const node&, const auto& callback);
    constexpr static inline bool matches(pattern_context&, const node&, const auto& callback);
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

template<auto str>
constexpr static bool check_or_insert_pattern_context(pattern_context& ctx,
                                                      const node& n,
                                                      const auto& node,
                                                      const auto& callback)
{
    constexpr static auto str_hash = hash(str.view());
    const auto node_hash = node_hasher(node);

    if (ctx.exists(str_hash, node_hash))
        return true;

    if (!ctx.can_be_inserted(str_hash, node_hash))
        return false;

    ctx.insert(str_hash, node_hasher(node), n);
    callback(ctx);
    return true;
}

constexpr static struct
{
    template<auto n>
    constexpr static bool operator()(pattern_context& ctx,
                                     const node&,
                                     const ce_const_value<n>&,
                                     const constant_node& constant,
                                     const auto& callback)
    {
        if (n == constant.value) {
            callback(ctx);
            return true;
        }
        return false;
    }

    template<auto left, auto right, operation_type type>
    constexpr static bool operator()(pattern_context& ctx,
                                     const node&,
                                     const ce_op_node<left, right, type>&,
                                     const op_node& op,
                                     const auto& callback)
    {
        if (op.type != type)
            return false;

        assert(op.left.get());
        assert(op.right.get());

        pattern_context ctx2;
        if constexpr (is_commutative(type))
            ctx2 = auto{ ctx };

        const auto normal_match = left.matches(ctx, *op.left, [](const auto&){}) && 
                                  right.matches(ctx, *op.right, [](const auto&){});
        if (normal_match) {
            callback(ctx);
            return true;
        }

        if constexpr (is_commutative(type)) {
            const auto commutative_match = left.matches(ctx2, *op.right, [](const auto&){}) &&
                                           right.matches(ctx2, *op.left, [](const auto&){});
            if (commutative_match) {
                callback(ctx2);
                return true;
            }
        }

        return false;
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx,
                                     const node& node,
                                     const ce_const_var<s>&,
                                     const constant_node& actual_node,
                                     const auto& callback)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node, callback);
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx,
                                     const node& node,
                                     const ce_const_var<s>&,
                                     const symbol_node& actual_node,
                                     const auto& callback)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node, callback);
    }

    template<auto s>
    constexpr static bool operator()(pattern_context& ctx,
                                     const node& node,
                                     const ce_any_node<s>&,
                                     const auto& actual_node,
                                     const auto& callback)
    {
        return check_or_insert_pattern_context<s>(ctx, node, actual_node, callback);
    }

    constexpr static bool operator()(pattern_context&,
                                     const node&,
                                     const auto&,
                                     const auto&,
                                     const auto&)
    {
        return false;
    }

} ce_matcher;

template<auto ce_node>
constexpr bool pattern_impl<ce_node>::matches(pattern_context& ctx, const node& node, const auto& callback)
{
    return std::visit([&node, &ctx, &callback](const auto& actual_node) {
        return ce_matcher(ctx, node, ce_node, actual_node, callback);
    }, node);
}

template<auto ce_node>
constexpr inline bool pattern_impl<ce_node>::matches(const node& node, const auto& callback)
{
    pattern_context ctx;
    return matches(ctx, node, callback);
}

template<auto Pattern, auto rewriter>
struct extra_rewrites_t {
    constexpr static void operator()(op_node& op)
    {
        rewrite<Pattern, rewriter>(*op.left);
        rewrite<Pattern, rewriter>(*op.right);
    }

    constexpr static void operator()(function_call_node& fn)
    {
        for(auto& argument : fn.arguments)
            rewrite<Pattern, rewriter>(argument);
    }

    constexpr static void operator()(auto&) {}
};

template<auto Pattern, auto rewriter>
constexpr static inline void rewrite(node& node)
{
    if (Pattern.matches(node, [&node](const auto& ctx) { node = std::move(rewriter(ctx)); }))
        return;

    std::visit(extra_rewrites_t<Pattern, rewriter>{}, node);
}

// test

template<typename T>
constexpr static inline bool pattern_test(const std::string_view source, const T&)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_result = parser::parse(vec);
    assert(node_result.has_value());

    const auto& node = node_result.value();
    return T::matches(node, [](const auto&){});
}

#ifndef NO_TEST
static_assert(pattern_test("1", pattern::constant<1>()));
static_assert(pattern_test("1+1", pattern::constant<1>().add<pattern::constant<1>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<2>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<5>()>())); // commutativity
#endif

}
