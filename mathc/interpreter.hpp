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
    constexpr static execution_result run(const node& root_node, vm& vm);
    constexpr static execution_result simplify(const node& root_node, vm& vm);
};

// Implementation

constexpr inline execution_result interpreter::simplify(const node& root_node, vm& vm)
{
    const struct
    {
        const node& root_node;
        struct vm& vm;

        constexpr auto operator()(const op_node& op) const
        {
            TRY(left_result, interpreter::simplify(*op.left, vm));
            TRY(right_result, interpreter::simplify(*op.right, vm));

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

            if (std::holds_alternative<node>(left_result) ||
                std::holds_alternative<node>(right_result)) {
                return make_execution_result<op_node>(std::visit(creator, std::move(left_result)),
                                                      std::visit(creator, std::move(right_result)),
                                                      op.type);
            }

            const auto& left_result_number = std::get<number>(left_result);
            const auto& right_result_number = std::get<number>(right_result);

            switch(op.type) {
                case operation_type::mul: return make_execution_result<number>(left_result_number * right_result_number);
                case operation_type::div: return make_execution_result<number>(left_result_number / right_result_number);
                case operation_type::add: return make_execution_result<number>(left_result_number + right_result_number);
                case operation_type::sub: return make_execution_result<number>(left_result_number - right_result_number);
                case operation_type::exp: return make_execution_result<number>(left_result_number ^ right_result_number);
            }

            std::unreachable();
        }

        constexpr auto operator()(const constant_node& c) const
        {
            return execution_result{ c.value };
        }

        constexpr auto operator()(const symbol_node& symbol) const
        {
            auto symbol_node = vm.symbol_node(symbol.value);
            if (symbol_node.has_value())
                return simplify(symbol_node.value(), vm);

            return make_execution_result<node>(copy_node(root_node));
        }

        constexpr auto operator()(const function_call_node& function_call) const
        {
            const auto function = find_function(function_call.function_name);
            if (!function)
                return make_execution_error(std::format("Function {} not found.", function_call.function_name));

            std::vector<number> results{};
            results.reserve(function_call.arguments.size());

            for(auto i = 0u; i < function_call.arguments.size(); i++) {
                const auto& argument = function_call.arguments[i];
                TRY(simplified, simplify(argument, vm));
                if (!std::holds_alternative<number>(simplified))
                    break;

                results.emplace_back(std::get<number>(simplified));
            }

            if (results.size() == function_call.arguments.size())
                return function->func(results);

            std::vector<node> new_arguments{};
            new_arguments.reserve(function_call.arguments.size());
            for(auto i = 0u; i < function_call.arguments.size(); i++) {
                if (i < results.size())
                    new_arguments.emplace_back(make_node<constant_node>( results[i] ));
                else
                    new_arguments.emplace_back(copy_node(function_call.arguments[i]));
            }

            return make_execution_result<function_call_node>(function_call.function_name, std::move(new_arguments));
        }
    } simplify_visitor{ root_node, vm };

    return std::visit(simplify_visitor, root_node);
}

}
