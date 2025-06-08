#pragma once

#include <span>

#include <number.hpp>
#include <common.hpp>

namespace mathc
{

constexpr static inline execution_result sqrt(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("sqrt expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(std::sqrt(arg.promote_to_double()));
}
constexpr static inline execution_result log2(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("log2 expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(std::log2(arg.promote_to_double()));
}
constexpr static inline execution_result ln(const std::span<number> args)
{
    if (args.size() > 1 || args.size() < 1)
        return make_execution_error(std::format("ln expects 1 argument, got {}", args.size()));

    const auto& arg = args[0];
    return make_execution_result<number>(std::log(arg.promote_to_double()));
}

constexpr static const auto functions = std::array {
    function{ "sqrt",  sqrt },
    function{ "log2",  log2 },
    function{ "ln",    ln   },
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
