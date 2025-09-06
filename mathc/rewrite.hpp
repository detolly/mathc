#pragma once

#include <node.hpp>
#include <pattern.hpp>

namespace mathc
{

using pattern_rewriter_fn_t = bool(&)(node&);
using rewriter_algorithm_fn_t = bool(&)(pattern_rewriter_fn_t, node&);

template<auto _pattern, auto rewriter>
struct pattern_strategy
{
    constexpr static bool try_rewrite(node& node)
    {
        pattern_context<_pattern.extent()> ctx;
        if (_pattern.matches(ctx, node)) {
            rewriter(ctx);
            return true;
        }

        return false;
    }
};

struct extra_rewrites_t
{
    rewriter_algorithm_fn_t rewriter_algorithm;
    pattern_rewriter_fn_t rewriter;

    constexpr inline bool operator()(op_node& op)
    {
        const auto a = rewriter_algorithm(rewriter, *op.left);
        const auto b = rewriter_algorithm(rewriter, *op.right);
        return a || b;
    }

    constexpr inline bool operator()(function_call_node& fn)
    {
        bool did_rewrite = false;
        for(auto& argument : fn.arguments)
            did_rewrite = rewriter_algorithm(rewriter, argument) || did_rewrite;

        return did_rewrite;
    }

    constexpr inline bool operator()(auto&) { return false; }
};

constexpr static bool _top_down_rewrite(pattern_rewriter_fn_t fn, node& node)
{
    return fn(node) || std::visit(extra_rewrites_t{ _top_down_rewrite, fn }, node);
}

constexpr static bool _bottom_up_rewrite(pattern_rewriter_fn_t fn, node& node)
{
    const auto did_rewrite = std::visit(extra_rewrites_t{ _bottom_up_rewrite, fn }, node);
    return fn(node) || did_rewrite;
}

template<pattern_strategy... strategies>
struct rewriter
{
    constexpr static void top_down_rewrite(node& node)
    {
        while ((_top_down_rewrite(strategies.try_rewrite, node) || ...)) {
            if !consteval {
                print_tree(node);
                std::puts("");
            }
        }
    }

    constexpr static void bottom_up_rewrite(node& node)
    {
        while ((_bottom_up_rewrite(strategies.try_rewrite, node) || ...)) {
            if !consteval {
                print_tree(node);
                std::puts("");
            }
        }
    }
};

}
