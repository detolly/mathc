#pragma once

#include <common.hpp>
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

template<fixed_string name>
constexpr static auto&& get(const auto& ctx) { return std::move(ctx.template get<name>()); }

#define move_in_hierarchy(x, y)                 \
        do { node _x = node{ std::move(x) };    \
        y = std::move(_x); } while(0)

constexpr static auto strategies = patterns<
    // x + 0 = x
    p<pattern::var<"x">().add<"op">(pattern::constant<0>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x - 0 = x
    p<pattern::var<"x">().sub<"op">(pattern::constant<0>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x / 1 = x
    p<pattern::var<"x">().div<"op">(pattern::constant<1>()), [](const auto& ctx) {
        move_in_hierarchy(get<"op">(ctx), get<"x">(ctx));
    }>{},
    // x / x = 1
    p<pattern::var<"x">().div<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},
    // x * 1 = x
    p<pattern::var<"x">().mul<"op">(pattern::constant<1>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x * 0 = 0
    p<pattern::var<"x">().mul<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(0));
    }>{},

    // 1 / (1 / x) = x
    p<pattern::constant<1>().div<"op">(pattern::constant<1>().div(pattern::var<"x">())), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // (1 / x) * x = 1
    p<pattern::constant<1>().div(pattern::var<"x">()).mul<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},

    // x * x = x^2
    p<pattern::var<"x">().mul<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<op_node>(std::make_unique<node>(get<"x">(ctx)),
                                            make_unique_node<constant_node>(number::from_int(2)),
                                            operation_type::exp);
    }>{},
    // (x^y)*x = x^(y+1)
    p<pattern::var<"x">().exp<"exp">(pattern::var<"y">()).mul<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get<"y">(ctx) = make_node<op_node>(std::make_unique<node>(std::move(get<"y">(ctx))),
                                           make_unique_node<constant_node>(number::from_int(1)),
                                           operation_type::add);
        move_in_hierarchy(get<"exp">(ctx), get<"op">(ctx));
    }>{},

    // sqrt(x)^2 = x
    p<pattern::func<"sqrt">(pattern::var<"x">()).exp<"op">(pattern::constant<2>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},

    // function invocations
    p<pattern::func<"sqrt", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get<"fn">(ctx) = make_node<constant_node>(math::sqrt(std::get<constant_node>(get<"x">(ctx)).value.to_double()));
    }>{},
    p<pattern::func<"log2", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get<"fn">(ctx) = make_node<constant_node>(math::log2(std::get<constant_node>(get<"x">(ctx)).value.to_double()));
    }>{},
    p<pattern::func<"ln", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get<"fn">(ctx) = make_node<constant_node>(math::ln(std::get<constant_node>(get<"x">(ctx)).value.to_double()));
    }>{},

    // operations
#define op(type) get<"op">(ctx) = operate(get<"a">(ctx), get<"b">(ctx), type);

    p<pattern::cvar<"a">().add<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::add) }>{},
    p<pattern::cvar<"a">().mul<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::mul) }>{},
    p<pattern::cvar<"a">().div<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::div) }>{},
    p<pattern::cvar<"a">().sub<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::sub) }>{},
    p<pattern::cvar<"a">().exp<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::exp) }>{}

#undef op

    //distribute
    // c * (x+y)
    // p<pattern::cvar<"c">().mul<"mul">(pattern::op<operation_type::add, "op">(pattern::var<"x">(), pattern::var<"y">())), [](const auto& ctx) {
    //
    //     return;
    // }>{},
    
>{};


constexpr static void simplify(node& node, vm&)
{
    while(strategies.execute(node)) {
        if !consteval {
            print_tree(node);
            std::puts("");
        }
    }
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
static_assert(simplify_test("4*5", "20"));
static_assert(simplify_test("1*4", "4"));
static_assert(simplify_test("1/(1/x)", "x"));
static_assert(simplify_test("(a^y)*a", "a^(y+1)"));
static_assert(simplify_test("1+1", "2"));
static_assert(simplify_test("sqrt(x)*sqrt(x)", "x"));
#endif

}
