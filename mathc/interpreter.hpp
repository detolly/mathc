#pragma once

#include <cmath>
#include <string_view>
#include <variant>

#include <parser.hpp>

namespace mathc
{

struct symbol_table
{
    struct node_t
    {

    };

    constexpr auto find(const std::string_view) const { return 0; }
};

using execution_result = int;

struct interpreter
{
    constexpr static execution_result interpret(const node& root_node,
                                                const symbol_table& symbol_table);
};

constexpr execution_result interpreter::interpret(const node& root_node,
                                                  const symbol_table& symbol_table)
{
    if(std::holds_alternative<op_node>(root_node)) {
        const auto& op = std::get<op_node>(root_node);
        const auto left_result = interpreter::interpret(*op.left, symbol_table);
        const auto right_result = interpreter::interpret(*op.right, symbol_table);
        switch(op.type) {
            case operation_type::mul: return execution_result{ left_result * right_result };
            case operation_type::div: return execution_result{ left_result / right_result };
            case operation_type::add: return execution_result{ left_result + right_result };
            case operation_type::sub: return execution_result{ left_result - right_result };
            case operation_type::exp: return execution_result{ static_cast<int>(std::pow(left_result, right_result)) };
        }

        std::unreachable();
    }

    else if (std::holds_alternative<constant_node>(root_node)) {
        const auto& op = std::get<constant_node>(root_node);
        return execution_result{ op.value };
    }

    else if(std::holds_alternative<symbol_node>(root_node)) {
        const auto& op = std::get<symbol_node>(root_node);
        return execution_result{ symbol_table.find(op.value) };
    }

    std::unreachable();
}

}
