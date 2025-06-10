#pragma once

#include <span>

#include <common.hpp>
#include <math.hpp>
#include <number.hpp>

namespace mathc
{

constexpr static inline execution_result vm_sqrt(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("sqrt expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(math::sqrt(arg.promote_to_double()));
}

constexpr static inline execution_result vm_log2(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("log2 expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(math::log2(arg.promote_to_double()));
}

constexpr static inline execution_result vm_ln(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("ln expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(math::log(arg.promote_to_double()));
}

constexpr static const auto functions =
{
    function{ "sqrt",  vm_sqrt },
    function{ "log2",  vm_log2 },
    function{ "ln",    vm_ln   },
};

[[nodiscard]]
constexpr static inline const function* find_function(const std::string_view function)
{
    for(const auto& f : functions)
        if (f.name == function)
            return &f;

    return nullptr;
}

}
