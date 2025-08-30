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

#define node_template \
template<template<typename T> typename ptr, template<typename T> typename vector, typename string>

node_template struct op_node_t;
node_template struct constant_node_t;
node_template struct symbol_node_t;
node_template struct function_call_node_t;

node_template
using node_t = std::variant<op_node_t<ptr, vector, string>,
                            constant_node_t<ptr, vector, string>,
                            symbol_node_t<ptr, vector, string>,
                            function_call_node_t<ptr, vector, string>>;

#define template_node_t node_t<ptr, vector, string>

template<typename T> using node_ptr_t    = std::unique_ptr<T>;
template<typename T> using node_vector_t = std::vector<T>;
                     using node_string_t = std::string;

#define runtime_node_template_arguments node_ptr_t, node_vector_t, node_string_t
using node = node_t<runtime_node_template_arguments>;

node_template struct constant_node_t
{
    number value;
};

node_template struct symbol_node_t
{
    string value;
};

node_template struct function_call_node_t
{
    string function_name;
    vector<template_node_t> arguments;
};

node_template struct op_node_t
{
    ptr<template_node_t> left;
    ptr<template_node_t> right;
    operation_type type;
};

using constant_node = constant_node_t<runtime_node_template_arguments>;
using symbol_node = symbol_node_t<runtime_node_template_arguments>;
using function_call_node = function_call_node_t<runtime_node_template_arguments>;
using op_node = op_node_t<runtime_node_template_arguments>;


// Utilities


template<typename T>
concept node_type = []<typename... Ts>(std::variant<Ts...>) {
    return (std::same_as<T, Ts> || ...);
}(node{});

template<node_type T, typename... Args>
constexpr static inline node make_node(Args&&... args)
{
    return node{ std::in_place_type_t<T>{}, std::forward<Args>(args)... };
}

template<node_type T, typename... Args>
constexpr static inline std::unique_ptr<node> make_unique_node(Args&&... args)
{
    return std::make_unique<node>(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
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

constexpr static auto copy_function_node(const function_call_node& f)
{
    return make_node<function_call_node>(f.function_name,
                                         copy_arguments(f));
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
        return copy_function_node(op);
    }
    constexpr static auto operator()(const symbol_node& op) { return make_node<symbol_node>(op); }
    constexpr static auto operator()(const constant_node& op) { return make_node<constant_node>(op); }
} copy_visitor{};

constexpr static inline node copy_node(const auto& n)
{
    return std::visit(copy_visitor, n);
}

constexpr static inline std::string_view operation_type_to_string_view(operation_type type)
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

}
