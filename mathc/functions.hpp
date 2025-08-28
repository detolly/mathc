#pragma once

#include <span>

#include <common.hpp>
#include <math.hpp>
#include <number.hpp>

namespace mathc
{

struct function
{
    std::string_view name;
    std::optional<node>(&func)(const std::span<node> numbers);
};

constexpr static inline std::optional<node> vm_sqrt(const std::span<node> args)
{
    if (args.size() > 1 || args.size() < 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};

    return make_node<constant_node>(math::sqrt(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static inline std::optional<node> vm_log2(const std::span<node> args)
{
    if (args.size() > 1 || args.size() < 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};

    return make_node<constant_node>(math::log2(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static inline std::optional<node> vm_ln(const std::span<node> args)
{
    if (args.size() > 1 || args.size() < 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};
    
    return make_node<constant_node>(math::log(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static const auto functions =
{
    function{ "sqrt"sv,  vm_sqrt },
    function{ "log2"sv,  vm_log2 },
    function{ "ln"sv,    vm_ln   },
};

[[nodiscard]]
constexpr static inline const std::optional<function> find_function(const std::string_view function)
{
    for(const auto& f : functions)
        if (f.name == function)
            return f;

    return {};
}

}
