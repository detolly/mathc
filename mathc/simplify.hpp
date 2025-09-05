#pragma once

#include <cmath>
#include <common.hpp>
#include <node.hpp>
#include <number.hpp>
#include <pattern.hpp>
#include <print.hpp>
#include <vm.hpp>

namespace mathc
{

template<auto _pattern, auto rewriter, bool should_match_commutative>
struct pattern_strategy
{
    constexpr static bool try_rewrite(node& node)
    {
        pattern_context<_pattern.extent()> ctx;
        if (_pattern.template matches<should_match_commutative>(ctx, node)) {
            rewriter(ctx);
            return true;
        }

        return false;
    }
};

// NOTE: Top down

template<pattern_strategy strategy>
constexpr static bool top_down_rewrite(node& node);

template<pattern_strategy strategy>
struct top_down_rewriter
{
    constexpr inline bool operator()(op_node& op)
    {
        const bool did_rewrite = top_down_rewrite<strategy>(*op.left) ||
                                 top_down_rewrite<strategy>(*op.right);

        return did_rewrite;
    }

    constexpr inline bool operator()(function_call_node& fn)
    {
        for(auto& argument : fn.arguments)
            if (top_down_rewrite<strategy>(argument))
                return true;

        return false;
    }

    constexpr inline bool operator()(auto&) { return false; }
};

template<pattern_strategy strategy>
constexpr static bool top_down_rewrite(node& node)
{
    if (strategy.try_rewrite(node))
        return true;

    return std::visit(top_down_rewriter<strategy>{}, node);
}

template<pattern_strategy strategy>
constexpr static bool top_down_rewrite_loop(node& node)
{
    bool did_rewrite = false;
    while(top_down_rewrite<strategy>(node)) {
        if !consteval {
            print_tree(node);
            std::puts("");
        }
        did_rewrite = true;
    }

    return did_rewrite;
}

// NOTE: Bottom up

template<pattern_strategy strategy>
constexpr static bool bottom_up_rewrite(node& node);

template<pattern_strategy strategy>
struct bottom_up_rewriter
{
    constexpr inline bool operator()(op_node& op)
    {
        const bool a = bottom_up_rewrite<strategy>(*op.left);
        const bool b = bottom_up_rewrite<strategy>(*op.right);
        return a || b;
    }

    constexpr inline bool operator()(function_call_node& fn)
    {
        bool did_rewrite = false;
        for(auto& argument : fn.arguments)
            did_rewrite = bottom_up_rewrite<strategy>(argument) || did_rewrite;
        return did_rewrite;
    }

    constexpr inline bool operator()(auto&) { return false; }
};

template<pattern_strategy strategy>
constexpr static bool bottom_up_rewrite(node& node)
{
    const auto did_rewrite = std::visit(bottom_up_rewriter<strategy>{}, node);
    return strategy.try_rewrite(node) || did_rewrite;
}

template<pattern_strategy... strategies>
struct patterns
{
    constexpr static void top_down_rewrite(node& node)
    {
        if ((top_down_rewrite_loop<strategies>(node) || ...))
            top_down_rewrite(node);
    }

    constexpr static void bottom_up_rewrite(node& node)
    {
        if ((bottom_up_rewrite<strategies>(node) || ...)) {
            if !consteval {
                print_tree(node);
                std::puts("");
            }
            bottom_up_rewrite(node);
        }
    }
};

template<auto a, auto b, bool should_match_commutative = true>
using p = pattern_strategy<a, b, should_match_commutative>;

template<fixed_string name>
constexpr static auto& get(const auto& ctx) { return ctx.template get<name>(); }

#define move_in_hierarchy(x, y)                 \
        do { node _x = node{ std::move(x) };    \
        y = std::move(_x); } while(0)

// TODO: Expand -> Reorder -> Simplify is probably a better strategy.

// FIXME: most if not all of these strategies shouldn't have to malloc.
constexpr static auto strategies = patterns<
    // NOTE: Reorder

    // var * c = c * var
    p<pattern::var<"x">().mul<"mul1">(pattern::cvar<"c">()), [](const auto& ctx) {
        auto&& mul1_op = std::get<op_node>(get<"mul1">(ctx));
        std::swap(mul1_op.left, mul1_op.right);
    }, false>{},

    // c * (c2*x) = (c * c2) * x
    p<pattern::cvar<"c">().mul<"mul1">(pattern::op<operation_type::mul, "mul2">(pattern::cvar<"c2">(), pattern::var<"x">())), [](const auto& ctx) {
        auto op2 = make_unique_node<op_node>(std::make_unique<node>(std::move(get<"c">(ctx))),
                                             std::make_unique<node>(std::move(get<"c2">(ctx))),
                                             operation_type::mul);

        get<"mul1">(ctx) = make_node<op_node>(std::move(op2),
                                              std::make_unique<node>(std::move(get<"x">(ctx))),
                                              operation_type::mul);
    }>{},

    // NOTE: identities

    // x + 0 = x
    p<pattern::any<"x">().add<"op">(pattern::constant<0>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x - 0 = x
    p<pattern::any<"x">().sub<"op">(pattern::constant<0>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x / 1 = x
    p<pattern::any<"x">().div<"op">(pattern::constant<1>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // x / -1 = -1 * x
    p<pattern::any<"x">().div<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        std::get<op_node>(get<"op">(ctx)).type = operation_type::mul;
    }>{},
    // x * 1 = x
    p<pattern::any<"x">().mul<"op">(pattern::constant<1>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // c * -1 = -c
    p<pattern::cvar<"c">().mul<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        std::get<constant_node>(get<"c">(ctx)).value = std::get<constant_node>(get<"c">(ctx)).value * number::from_int(-1);
        move_in_hierarchy(get<"c">(ctx), get<"op">(ctx));
    }>{},
    // x / x = 1
    p<pattern::any<"x">().div<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},
    // x * 0 = 0
    p<pattern::any<"x">().mul<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(0));
    }>{},
    // x / 0 = nan
    p<pattern::any<"x">().div<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_double(static_cast<double>(NAN)));
    }>{},
    // x ^ 0 = 1
    p<pattern::any<"x">().exp<"op">(pattern::constant<0>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},
    // x ^ 1 = x
    p<pattern::any<"x">().exp<"op">(pattern::constant<1>()), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},

    // -1 * -1 = 1
    p<pattern::constant<-1>().mul<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},

    // 0 ^ -1 = NaN
    p<pattern::constant<0>().exp<"op">(pattern::constant<-1>()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_double(static_cast<double>(NAN)));
    }>{},
    // 0 ^ c = 0 
    p<pattern::constant<0>().exp<"op">(pattern::cvar()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(0));
    }>{},

    // 0 / x = 0
    p<pattern::constant<0>().div<"op">(pattern::any()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(0));
    }>{},
    // 1 / (1 / x) = x
    p<pattern::constant<1>().div<"op">(pattern::constant<1>().div(pattern::any<"x">())), [](const auto& ctx) {
        move_in_hierarchy(get<"x">(ctx), get<"op">(ctx));
    }>{},
    // (1 / x) * x = 1
    p<pattern::constant<1>().div(pattern::any<"x">()).mul<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<constant_node>(number::from_int(1));
    }>{},
    // (x / y) * (z / w) = (x * z) / (y * w)
    p<pattern::any<"x">().div(pattern::any<"y">()).mul<"op">(pattern::any<"z">().div(pattern::any<"w">())), [](const auto& ctx) {
        auto first_mul = make_unique_node<op_node>(std::make_unique<node>(std::move(get<"x">(ctx))),
                                                   std::make_unique<node>(std::move(get<"z">(ctx))),
                                                   operation_type::mul);

        auto second_mul = make_unique_node<op_node>(std::make_unique<node>(std::move(get<"y">(ctx))),
                                                    std::make_unique<node>(std::move(get<"w">(ctx))),
                                                    operation_type::mul);

        get<"op">(ctx) = make_node<op_node>(std::move(first_mul),
                                            std::move(second_mul),
                                            operation_type::div);
    }>{},
    // (x / y) + (z / y) = (x + z) / (y)
    p<pattern::any<"x">().div(pattern::any<"y">()).add<"op">(pattern::any<"z">().div(pattern::any<"y">())), [](const auto& ctx) {
        auto add_op = make_unique_node<op_node>(std::make_unique<node>(std::move(get<"x">(ctx))),
                                                std::make_unique<node>(std::move(get<"z">(ctx))),
                                                operation_type::add);

        get<"op">(ctx) = make_node<op_node>(std::move(add_op),
                                            std::make_unique<node>(std::move(get<"y">(ctx))),
                                            operation_type::div);
    }>{},

    // (x^y) * (x^z) = x^(y+z)
    p<pattern::any<"x">().exp(pattern::any<"y">()).mul<"op">(pattern::any<"x">().exp(pattern::any<"z">())), [](const auto& ctx) {
        auto op2 = make_unique_node<op_node>(std::make_unique<node>(std::move(get<"y">(ctx))),
                                             std::make_unique<node>(std::move(get<"z">(ctx))),
                                             operation_type::add);

        get<"op">(ctx) = make_node<op_node>(std::make_unique<node>(std::move(get<"x">(ctx))),
                                            std::move(op2),
                                            operation_type::exp);
    }>{},
    // x * x = x^2
    p<pattern::var<"x">().mul<"op">(pattern::var<"x">()), [](const auto& ctx) {
        get<"op">(ctx) = make_node<op_node>(std::make_unique<node>(std::move(get<"x">(ctx))),
                                            make_unique_node<constant_node>(number::from_int(2)),
                                            operation_type::exp);
    }>{},
    // (x^y)*x = x^(y+1)
    p<pattern::any<"x">().exp<"exp">(pattern::any<"y">()).mul<"op">(pattern::any<"x">()), [](const auto& ctx) {
        get<"y">(ctx) = make_node<op_node>(std::make_unique<node>(std::move(get<"y">(ctx))),
                                           make_unique_node<constant_node>(number::from_int(1)),
                                           operation_type::add);
        move_in_hierarchy(get<"exp">(ctx), get<"op">(ctx));
    }>{},

    // sqrt(x) = x^(1/2)
    p<pattern::func<"sqrt", "op">(pattern::any<"x">()), [](const auto& ctx) {
        auto one_over_two = make_unique_node<op_node>(make_unique_node<constant_node>(number::from_int(1)),
                                                      make_unique_node<constant_node>(number::from_int(2)),
                                                      operation_type::div);

        auto x = auto{ std::move(get<"x">(ctx)) };
        get<"op">(ctx) = make_node<op_node>(std::make_unique<node>(std::move(x)),
                                            std::move(one_over_two),
                                            operation_type::exp);
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
    p<pattern::cvar<"a">().exp<"op">(pattern::cvar<"b">()), [](const auto& ctx) { op(operation_type::exp) }>{},

#undef op

    // NOTE: distribute

    // c * (x+y) = (c*x+c*y)
    p<pattern::cvar<"c">().mul<"mul">(pattern::op<operation_type::add, "op">(pattern::any<"x">(), pattern::any<"y">())), [](const auto& ctx) {
        auto&& add_node = get<"op">(ctx);
        auto& add_op = std::get<op_node>(add_node);

        *add_op.left = make_node<op_node>(std::make_unique<node>(copy_node(get<"c">(ctx))),
                                         std::make_unique<node>(std::move(*add_op.left)),
                                         operation_type::mul);

        *add_op.right = make_node<op_node>(std::make_unique<node>(std::move(get<"c">(ctx))),
                                           std::make_unique<node>(std::move(*add_op.right)),
                                           operation_type::mul);

        move_in_hierarchy(add_node, get<"mul">(ctx));
    }>{}

>{};


constexpr static void simplify(node& node, vm&)
{
    strategies.bottom_up_rewrite(node);
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
static_assert(simplify_test("-a*-a", "a^2"));
static_assert(simplify_test("x^(-1)", "1/x"));
static_assert(simplify_test("(a^0)^0", "1"));
static_assert(simplify_test("-0", "0"));
static_assert(simplify_test("(a/b)/(a/b)", "1"));
#endif

}
