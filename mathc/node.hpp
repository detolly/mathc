#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

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

struct op_node
{
    std::unique_ptr<node> left;
    std::unique_ptr<node> right;
    operation_type type;
};

template<typename T>
concept node_type = []<typename... Ts>(std::variant<Ts...>) {
    return (std::same_as<T, Ts> || ...);
}(node{});

template<node_type T, typename... Args>
constexpr static inline node make_node(Args&&... args)
{
    return node{ std::in_place_type_t<T>{}, std::forward<Args>(args)... };
}

constexpr static inline node copy_node(const auto& n);
constexpr static inline std::vector<node> copy_arguments(const function_call_node& op)
{
    std::vector<node> arguments;
    arguments.reserve(op.arguments.size());

    for(const auto& argument_node : op.arguments)
        arguments.emplace_back(copy_node(argument_node));

    return arguments;
}

constexpr static struct
{
    constexpr static auto operator()(const op_node& op)
    {
        return make_node<op_node>(std::make_unique<node>(copy_node(*op.left)),
                                  std::make_unique<node>(copy_node(*op.right)),
                                  op.type);
    }
    constexpr static auto operator()(const function_call_node& op)
    {
        return make_node<function_call_node>(op.function_name,
                                             copy_arguments(op));
    }
    constexpr static auto operator()(const symbol_node& op) { return make_node<symbol_node>(op); }
    constexpr static auto operator()(const constant_node& op) { return make_node<constant_node>(op); }
} copy_visitor{};

constexpr static inline node copy_node(const auto& n)
{
    return std::visit(copy_visitor, n);
}

}
