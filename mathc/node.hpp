#pragma once

#include <string_view>
#include <string>
#include <utility>
#include <variant>
#include <memory>

#include <number.hpp>

namespace mathc
{

using namespace std::string_view_literals;

enum class operation_type
{
    mul,
    div,
    add,
    sub,
    exp
};

constexpr static inline auto operation_type_to_string(operation_type type)
{
    switch(type) {
        case operation_type::mul: return "*"sv;
        case operation_type::div: return "/"sv;
        case operation_type::add: return "+"sv;
        case operation_type::sub: return "-"sv;
        case operation_type::exp: return "^"sv;
    }

    std::unreachable();
}

struct op_node;
struct constant_node;
struct symbol_node;
struct function_call_node;

using node = std::variant<op_node, constant_node, symbol_node, function_call_node>;

struct op_node
{
    std::unique_ptr<node> left;
    std::unique_ptr<node> right;
    operation_type type;
};

struct constant_node
{
    number value;
};

struct symbol_node
{
    std::string value;
};

struct function_call_node
{
    std::string function_name;
    std::vector<node> arguments;
};

}
