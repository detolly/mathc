#pragma once

#include <common.hpp>
#include <functions.hpp>
#include <node.hpp>
#include <number.hpp>
#include <pattern.hpp>
#include <print.hpp>
#include <vm.hpp>

namespace mathc
{

template<auto Pattern, auto rewriter>
struct pattern_strategy
{
    constexpr bool execute(node& n) const { return rewrite<Pattern, rewriter>(n); }
};

template<pattern_strategy strategy, pattern_strategy... strategies>
struct patterns
{
    constexpr static bool execute(node& n)
    {
        bool did_rewrite;
        if constexpr (sizeof...(strategies) > 0) did_rewrite = strategy.execute(n) ||
                                                               patterns<strategies...>::execute(n);
        else did_rewrite = strategy.execute(n);
        return did_rewrite;
    }
};

template<auto a, auto b>
using p = pattern_strategy<a, b>;

constexpr static auto strategies = patterns<
    // x + 0 = x
    p<pattern::var<"x">().add<pattern::constant<0>(), "op">(), [](const auto& ctx) {
        node var = std::move(ctx.template get<"x">());
        ctx.template get<"op">() = std::move(var);
    }>{},
    // x * 1 = x
    p<pattern::var<"x">().mul<pattern::constant<1>(), "op">(), [](const auto& ctx) {
        node var = auto{ std::move(ctx.template get<"x">()) };
        ctx.template get<"op">() = std::move(var);
    }>{},
    // x * x = x^2
    p<pattern::var<"x">().mul<pattern::var<"x">(), "op">(), [](const auto& ctx) {
        ctx.template get<"op">() = make_node<op_node>(std::make_unique<node>(std::move(ctx.template get<"x">())),
                                                      make_unique_node<constant_node>(number::from_int(2)),
                                                      operation_type::exp);
    }>{},
    // (x^y)*x = x^(y+1)
    p<pattern::var<"x">().exp<pattern::cvar<"y">(), "exp">().mul<pattern::var<"x">(), "op">(), [](const auto& ctx) {
        ctx.template get<"y">() = make_node<op_node>(std::make_unique<node>(std::move(ctx.template get<"y">())),
                                                     make_unique_node<constant_node>(number::from_int(1)),
                                                     operation_type::add);
        auto x = auto{ std::move(ctx.template get<"exp">()) };
        ctx.template get<"op">() = std::move(x);
    }>{}
>{};

constexpr static void simplify(node& node, vm&)
{
    while(strategies.execute(node)) {}
}

// test

constexpr static inline bool simplify_test(const std::string_view source,
                                           const std::string_view source2)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);
    auto node_result = parser::parse(vec);
    assert(node_result.has_value());
    auto& node = node_result.value();
    
    vm vm;
    simplify(node, vm);

    const auto vec2 = lexer::lex(source2);
    assert(vec2.size() > 0);
    auto node_result2 = parser::parse(vec2);
    assert(node_result2.has_value());
    auto& node2 = node_result2.value();

    return hash(node2) == hash(node);
}

#ifndef NO_TEST
static_assert(simplify_test("a*a", "a^2"));
static_assert(simplify_test("4+0", "4"));
static_assert(simplify_test("4*1", "4"));
static_assert(simplify_test("(a^y)*a", "a^(y+1)"));
#endif

}
