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

struct interpreter
{
    constexpr static node simplify(const node& root_node, vm& vm);
};

// Implementation

constexpr inline node interpreter::simplify(const node& root_node, vm& vm)
{
    const struct
    {
        const node& root_node;
        struct vm& vm;

        constexpr auto operator()(const op_node& op) const
        {
            auto left_result = interpreter::simplify(*op.left, vm);
            auto right_result = interpreter::simplify(*op.right, vm);

            constexpr static struct {
                constexpr static auto operator()(node&& n)
                {
                    return std::make_unique<node>(std::forward<node>(n));
                }
                constexpr static auto operator()(number&& n)
                {
                    return std::make_unique<node>(std::in_place_type_t<constant_node>{}, n);
                }
            } creator{};

            if (!std::holds_alternative<constant_node>(left_result) ||
                !std::holds_alternative<constant_node>(right_result)) {
                return make_node<op_node>(std::visit(creator, std::move(left_result)),
                                          std::visit(creator, std::move(right_result)),
                                          op.type);
            }

            const auto& left_result_number = std::get<constant_node>(left_result);
            const auto& right_result_number = std::get<constant_node>(right_result);

            switch(op.type) {
                case operation_type::mul:
                    return make_node<constant_node>(left_result_number.value * right_result_number.value);
                case operation_type::div:
                    return make_node<constant_node>(left_result_number.value / right_result_number.value);
                case operation_type::add:
                    return make_node<constant_node>(left_result_number.value + right_result_number.value);
                case operation_type::sub:
                    return make_node<constant_node>(left_result_number.value - right_result_number.value);
                case operation_type::exp:
                    return make_node<constant_node>(left_result_number.value ^ right_result_number.value);
            }

            std::unreachable();
        }

        constexpr auto operator()(const constant_node& c) const
        {
            return make_node<constant_node>(c.value);
        }

        constexpr auto operator()(const symbol_node& symbol) const
        {
            auto symbol_node = vm.symbol_node(symbol.value);
            if (symbol_node.has_value())
                return simplify(symbol_node.value(), vm);

            return copy_node(root_node);
        }

        constexpr auto operator()(const function_call_node& function_call) const
        {
            const auto function = find_function(function_call.function_name);
            if (!function.has_value())
                return copy_function_node(function_call);

            std::vector<node> simplified_arguments;
            simplified_arguments.reserve(function_call.arguments.size());
            for(auto i = 0u; i < function_call.arguments.size(); i++)
                simplified_arguments.emplace_back(simplify(function_call.arguments[i], vm));

            const auto result = function->func(simplified_arguments);
            if (result.has_value())
                return copy_node(result.value());

            return make_node<function_call_node>(function_call.function_name, std::move(simplified_arguments));
        }
    } simplify_visitor{ root_node, vm };

    return std::visit(simplify_visitor, root_node);
}

}
