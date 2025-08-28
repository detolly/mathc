#pragma once

#include <utility>
#include <variant>

#include <common.hpp>
#include <print.hpp>
#include <functions.hpp>
#include <node.hpp>
#include <number.hpp>
#include <vm.hpp>

namespace mathc
{

// Implementation

constexpr inline node operate(number a, number b, operation_type type)
{
    switch(type) {
        case operation_type::mul:
            return make_node<constant_node>(a * b);
        case operation_type::div:
            return make_node<constant_node>(a / b);
        case operation_type::add:
            return make_node<constant_node>(a + b);
        case operation_type::sub:
            return make_node<constant_node>(a - b);
        case operation_type::exp:
            return make_node<constant_node>(a ^ b);
    }
}

constexpr inline bool constant_fold(op_node& root_op)
{
    const auto constant_move = [](auto& node_to_check, auto& other_node, auto& to_replace) {
        if (!std::holds_alternative<constant_node>(*node_to_check))
            return false;

        if (std::holds_alternative<constant_node>(*to_replace))
            std::swap(other_node, to_replace);
        else
            std::swap(node_to_check, to_replace);

        return true;
    };

    const auto fold = [&constant_move](auto& root_node_to_check, auto& to_replace, auto root_op_type){
        if (!std::holds_alternative<op_node>(*root_node_to_check))
            return false;

        auto& checking_op = std::get<op_node>(*root_node_to_check);
        if (checking_op.type != root_op_type)
            return false;

        bool did_move = constant_move(checking_op.left, checking_op.right, to_replace);
        if (!did_move)
            did_move = constant_move(checking_op.right, checking_op.left, to_replace);

        return did_move;
    };

    bool did_fold = fold(root_op.right, root_op.left, root_op.type);
    if (!did_fold)
        did_fold = fold(root_op.left, root_op.right, root_op.type);

    return did_fold;
}

constexpr inline void simplify(node& root_node, vm& vm)
{
    const struct
    {
        node& root_node;
        struct vm& vm;

        constexpr auto operator()(op_node& root_op) const
        {
            simplify(*root_op.left, vm);
            simplify(*root_op.right, vm);

            if (!std::holds_alternative<constant_node>(*root_op.left) ||
                !std::holds_alternative<constant_node>(*root_op.right)) {
                if (constant_fold(root_op))
                    simplify(root_node, vm);
                return;
            }

            const auto left_result_number = std::get<constant_node>(*root_op.left).value;
            const auto right_result_number = std::get<constant_node>(*root_op.right).value;

            root_node = operate(left_result_number, right_result_number, root_op.type);
        }

        constexpr auto operator()(constant_node&) const
        {

        }

        constexpr auto operator()(symbol_node& symbol) const
        {
            auto symbol_node = vm.symbol_node(symbol.value);
            if (!symbol_node.has_value())
                return;

            auto& n = symbol_node.value();
            simplify(n, vm);
            root_node = std::move(n);
        }

        constexpr auto operator()(function_call_node& function_call) const
        {
            const auto function = find_function(function_call.function_name);
            if (!function.has_value())
                return;

            for(auto i = 0u; i < function_call.arguments.size(); i++)
                simplify(function_call.arguments[i], vm);

            auto result = function->func(function_call.arguments);
            if (result.has_value()) {
                root_node = std::move(result.value());
                return;
            }
        }
    } simplify_visitor{ root_node, vm };

    return std::visit(simplify_visitor, root_node);
}

}
