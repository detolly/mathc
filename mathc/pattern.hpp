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

    template<auto right, fixed_string name = ""> consteval static auto mul()
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::mul, name>{}>{};
    }

    template<auto right, fixed_string name = ""> consteval static auto div()
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::div, name>{}>{};
    }

    template<auto right, fixed_string name = ""> consteval static auto add()
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::add, name>{}>{};
    }

    template<auto right, fixed_string name = ""> consteval static auto sub()
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::sub, name>{}>{};
    }

    template<auto right, fixed_string name = ""> consteval static auto exp()
    {
        return pattern_impl<ce_op_node<pattern_impl<n>{}, right, operation_type::exp, name>{}>{};
    }

    constexpr static inline bool matches(auto& ctx, const node&);
};

struct pattern
{
    template<fixed_string s = "">
    constexpr static auto var() { return pattern_impl<ce_any_node<s>{}>{}; }

    template<fixed_string s = "">
    constexpr static auto cvar() { return pattern_impl<ce_const_var<s>{}>{}; }

    template<auto n, fixed_string s = "">
        requires(number_t<decltype(n)>)
    constexpr static auto constant() { return pattern_impl<ce_const_value<n, s>{}>{}; }
};

constexpr static struct
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
        if (left.matches(ctx, *op.left) && right.matches(ctx, *op.right)) {
            ctx.insert(hash(name.view()), hash(_node), _node);
            return true;
        }

        ctx.index = original_index;
        if constexpr (!is_commutative(type))
            return false;

        if (left.matches(ctx, *op.right) && right.matches(ctx, *op.left)) {
            ctx.insert(hash(name.view()), hash(_node), _node);
            return true;
        }

        ctx.index = original_index;
        return false;
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

} ce_matcher;

template<auto ce_node>
constexpr bool pattern_impl<ce_node>::matches(auto& ctx, const node& node)
{
    return std::visit([&node, &ctx](const auto& actual_node) {
        return ce_matcher(ctx, node, ce_node, actual_node);
    }, node);
}

template<auto Pattern, auto rewriter>
struct extra_rewrites_t {
    constexpr static bool operator()(op_node& op)
    {
        const auto a = rewrite<Pattern, rewriter>(*op.left);
        const auto b = rewrite<Pattern, rewriter>(*op.right);
        return a || b;
    }

    constexpr static bool operator()(function_call_node& fn)
    {
        bool did_rewrite = false;
        for(auto& argument : fn.arguments)
            did_rewrite = rewrite<Pattern, rewriter>(argument) || did_rewrite;

        return did_rewrite;
    }

    constexpr static bool operator()(auto&) { return false; }
};

template<auto Pattern, auto rewriter>
constexpr static inline bool rewrite(node& node)
{
    pattern_context<Pattern.extent()> ctx;
    if (Pattern.matches(ctx, node)) {
        rewriter(ctx);
        return true;
    }

    return std::visit(extra_rewrites_t<Pattern, rewriter>{}, node);
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
    return T::matches(ctx, node);
}

#ifndef NO_TEST
static_assert(pattern_test("1", pattern::constant<1>()));
static_assert(pattern_test("1", pattern::cvar()));
static_assert(pattern_test("1+1", pattern::constant<1>().add<pattern::constant<1>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<2>()>()));
static_assert(pattern_test("5*2", pattern::var<"x">().mul<pattern::constant<5>()>())); // commutativity
#endif

}
