#pragma once

#include <cmath>
#include <common.hpp>
#include <node.hpp>
#include <number.hpp>
#include <print.hpp>
#include <rewrite.hpp>
#include <vm.hpp>

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

template<auto _pattern, auto _rewriter>
using p = pattern_strategy<_pattern, _rewriter>;

template<fixed_string name>
constexpr static inline auto& get(const auto& ctx) { return ctx.template get<name>(); }

#define get(x) get<x>(ctx)
#define hierarchy_move(x, y) do { node _x = node{ std::move(x) }; y = std::move(_x); } while(0)

// TODO: Expand -> Reorder -> Simplify is probably a better strategy.

// FIXME: most if not all of these strategies shouldn't have to malloc.
constexpr static auto simplify_rewriter = rewriter<
    // NOTE: Reorder

    // not_c * c = c * not_c
    p<pattern::none_of<"x", constant_node>().mul<"mul1", match_commutative::no>(pattern::cvar<"c">()), [](const auto& ctx) {
        auto& mul1_op = std::get<op_node>(get("mul1"));
        std::swap(mul1_op.left, mul1_op.right);
    }>{},

    // c * (c2*x) = (c * c2) * x
    p<pattern::cvar<"c">().mul<"mul1">(pattern::op<operation_type::mul, "mul2">(pattern::cvar<"c2">(), pattern::none_of<"x", constant_node>())), [](const auto& ctx) {
        auto op2 = make_unique_node<op_node>(std::make_unique<node>(std::move(get("c"))),
                                             std::make_unique<node>(std::move(get("c2"))),
                                             operation_type::mul);

        get("mul1") = make_node<op_node>(std::move(op2),
                                         std::make_unique<node>(std::move(get("x"))),
                                         operation_type::mul);
    }>{},

    // NOTE: identities

    // x + 0 = x
    p<pattern::any<"x">().add<"op">(pattern::constant<0>()), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},
    // x - 0 = x
    p<pattern::any<"x">().sub<"op">(pattern::constant<0>()), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},
    // x / 1 = x
    p<pattern::any<"x">().div<"op">(pattern::constant<1>()), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},
    // x / -1 = -1 * x
    p<pattern::any<"x">().div<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        std::get<op_node>(get("op")).type = operation_type::mul;
    }>{},
    // x * 1 = x
    p<pattern::any<"x">().mul<"op">(pattern::constant<1>()), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},
    // c * -1 = -c
    p<pattern::cvar<"c">().mul<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        std::get<constant_node>(get("c")).value = std::get<constant_node>(get("c")).value * number::from_int(-1);
        hierarchy_move(get("c"), get("op"));
    }>{},
    // x / x = 1
    p<pattern::any<"x">().div<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(1));
    }>{},
    // x * 0 = 0
    p<pattern::any<"x">().mul<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(0));
    }>{},
    // x / 0 = nan
    p<pattern::any<"x">().div<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_double(static_cast<double>(NAN)));
    }>{},
    // x ^ 0 = 1
    p<pattern::any<"x">().exp<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(1));
    }>{},
    // x ^ 1 = x
    p<pattern::any<"x">().exp<"op">(pattern::constant<1>()), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},

    // -1 * -1 = 1
    p<pattern::constant<-1>().mul<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(1));
    }>{},

    // 0 ^ -1 = NaN
    p<pattern::constant<0>().exp<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_double(static_cast<double>(NAN)));
    }>{},
    // 0 ^ c = 0 
    p<pattern::constant<0>().exp<"op">(pattern::cvar()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(0));
    }>{},

    // 0 / x = 0
    p<pattern::constant<0>().div<"op">(pattern::any()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(0));
    }>{},
    // 1 / (1 / x) = x
    p<pattern::constant<1>().div<"op">(pattern::constant<1>().div(pattern::any<"x">())), [](const auto& ctx) {
        hierarchy_move(get("x"), get("op"));
    }>{},
    // (1 / x) * x = 1
    p<pattern::constant<1>().div(pattern::any<"x">()).mul<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get("op") = make_node<constant_node>(number::from_int(1));
    }>{},

    // NOTE: general simplifications

    // (x / y) * (z / w) = (x * z) / (y * w)
    p<pattern::any<"x">().div(pattern::any<"y">()).mul<"op">(pattern::any<"z">().div(pattern::any<"w">())), [](const auto& ctx) {
        auto first_mul = make_unique_node<op_node>(std::make_unique<node>(std::move(get("x"))),
                                                   std::make_unique<node>(std::move(get("z"))),
                                                   operation_type::mul);

        auto second_mul = make_unique_node<op_node>(std::make_unique<node>(std::move(get("y"))),
                                                    std::make_unique<node>(std::move(get("w"))),
                                                    operation_type::mul);

        get("op") = make_node<op_node>(std::move(first_mul),
                                       std::move(second_mul),
                                       operation_type::div);
    }>{},
    // (x / y) + (z / y) = (x + z) / (y)
    p<pattern::any<"x">().div(pattern::any<"y">()).add<"op">(pattern::any<"z">().div(pattern::any<"y">())), [](const auto& ctx) {
        auto add_op = make_unique_node<op_node>(std::make_unique<node>(std::move(get("x"))),
                                                std::make_unique<node>(std::move(get("z"))),
                                                operation_type::add);

        get("op") = make_node<op_node>(std::move(add_op),
                                       std::make_unique<node>(std::move(get("y"))),
                                       operation_type::div);
    }>{},

    // (x^y) * (x^z) = x^(y+z)
    p<pattern::any<"x">().exp(pattern::any<"y">()).mul<"op">(pattern::any<"x">().exp(pattern::any<"z">())), [](const auto& ctx) {
        auto op2 = make_unique_node<op_node>(std::make_unique<node>(std::move(get("y"))),
                                             std::make_unique<node>(std::move(get("z"))),
                                             operation_type::add);

        get("op") = make_node<op_node>(std::make_unique<node>(std::move(get("x"))),
                                            std::move(op2),
                                            operation_type::exp);
    }>{},
    // x * x = x^2
    p<pattern::var<"x">().mul<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get("op") = make_node<op_node>(std::make_unique<node>(std::move(get("x"))),
                                       make_unique_node<constant_node>(number::from_int(2)),
                                       operation_type::exp);
    }>{},
    // (x^y)*x = x^(y+1)
    p<pattern::any<"x">().exp<"exp">(pattern::any<"y">()).mul<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get("y") = make_node<op_node>(std::make_unique<node>(std::move(get("y"))),
                                      make_unique_node<constant_node>(number::from_int(1)),
                                      operation_type::add);
        hierarchy_move(get("exp"), get("op"));
    }>{},

    // sqrt(x) = x^(1/2)
    p<pattern::func<"sqrt", "op">(pattern::any<"x">()), [](const auto& ctx) {
        auto one_over_two = make_unique_node<op_node>(make_unique_node<constant_node>(number::from_int(1)),
                                                      make_unique_node<constant_node>(number::from_int(2)),
                                                      operation_type::div);

        auto x = auto{ std::move(get("x")) };
        get("op") = make_node<op_node>(std::make_unique<node>(std::move(x)),
                                       std::move(one_over_two),
                                       operation_type::exp);
    }>{},

    // function invocations
    p<pattern::func<"sqrt", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get("fn") = make_node<constant_node>(math::sqrt(std::get<constant_node>(get("x")).value.to_double()));
    }>{},
    p<pattern::func<"log2", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get("fn") = make_node<constant_node>(math::log2(std::get<constant_node>(get("x")).value.to_double()));
    }>{},
    p<pattern::func<"ln", "fn">(pattern::cvar<"x">()), [](const auto& ctx) {
        get("fn") = make_node<constant_node>(math::ln(std::get<constant_node>(get("x")).value.to_double()));
    }>{},

    // operations
#define op(type) get("op") = operate(get("a"), get("b"), type);

    p<pattern::cvar<"a">().add<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::add) }>{},
    p<pattern::cvar<"a">().mul<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::mul) }>{},
    p<pattern::cvar<"a">().div<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::div) }>{},
    p<pattern::cvar<"a">().sub<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::sub) }>{},
    p<pattern::cvar<"a">().exp<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::exp) }>{},

#undef op

    // NOTE: distribute

    // c * (x+y) = (c*x+c*y)
    p<pattern::cvar<"c">().mul<"mul">(pattern::op<operation_type::add, "op">(pattern::any<"x">(), pattern::any<"y">())), [](const auto& ctx) {
        auto&& add_node = get("op");
        auto& add_op = std::get<op_node>(add_node);

        *add_op.left = make_node<op_node>(std::make_unique<node>(copy_node(get("c"))),
                                          std::make_unique<node>(std::move(*add_op.left)),
                                          operation_type::mul);

        *add_op.right = make_node<op_node>(std::make_unique<node>(std::move(get("c"))),
                                           std::make_unique<node>(std::move(*add_op.right)),
                                           operation_type::mul);

        hierarchy_move(add_node, get("mul"));
    }>{}

>{};


constexpr static inline void simplify(node& node, vm&)
{
    simplify_rewriter.top_down_rewrite(node);
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
static_assert(simplify_test("5*(2*a)", "10*a"));
static_assert(simplify_test("a*10", "10*a"));
static_assert(simplify_test("x^0", "1"));
static_assert(simplify_test("x^1", "x"));

static_assert(simplify_test("0/1", "0"));
// static_assert(simplify_test("-a*-a", "a^2"));
// static_assert(simplify_test("x^(-1)", "1/x"));
static_assert(simplify_test("(a^0)^0", "1"));
static_assert(simplify_test("-0", "0"));
static_assert(simplify_test("(a/b)/(a/b)", "1"));
#endif

}
