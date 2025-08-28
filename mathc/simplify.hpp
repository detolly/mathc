#pragma once

#include <utility>
#include <variant>

#include <common.hpp>
#include <functions.hpp>
#include <node.hpp>
#include <number.hpp>
#include <vm.hpp>

namespace mathc
{

// Implementation

constexpr inline void simplify(node& root_node, vm& vm)
{
    const struct
    {
        node& root_node;
        struct vm& vm;

        constexpr auto operator()(op_node& op) const
        {
            simplify(*op.left, vm);
            simplify(*op.right, vm);

            if (!std::holds_alternative<constant_node>(*op.left) || !std::holds_alternative<constant_node>(*op.right))
                return;

            const auto left_result_number = std::get<constant_node>(*op.left);
            const auto right_result_number = std::get<constant_node>(*op.right);

            switch(op.type) {
                case operation_type::mul:
                    root_node = make_node<constant_node>(left_result_number.value * right_result_number.value);
                    break;
                case operation_type::div:
                    root_node = make_node<constant_node>(left_result_number.value / right_result_number.value);
                    break;
                case operation_type::add:
                    root_node = make_node<constant_node>(left_result_number.value + right_result_number.value);
                    break;
                case operation_type::sub:
                    root_node = make_node<constant_node>(left_result_number.value - right_result_number.value);
                    break;
                case operation_type::exp:
                    root_node = make_node<constant_node>(left_result_number.value ^ right_result_number.value);
                    break;
            }
        }

        constexpr auto operator()(constant_node&) const
        {
            return;
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
