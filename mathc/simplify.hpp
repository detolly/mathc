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

struct strategy
{
    constexpr void rewrite(node&) const;
};

template<auto Pattern, auto rewriter>
struct pattern_strategy : public strategy
{
    constexpr void execute(node& n) const { rewrite<Pattern, rewriter>(n); }
};

#define p(a, b) pattern_strategy<a, b>{}

constexpr static auto strategies = std::array
{
    p(pattern::var<"x">().mul<pattern::constant<1>()>(), [](const auto& ctx){ return ctx.template get<"x">(); }), // x * 1 = x
};

constexpr static void simplify(node& node, vm&)
{
    for(const auto& strategy : strategies)
        strategy.execute(node);
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

// static_assert(simplify_test("4*1", "4"));

}
