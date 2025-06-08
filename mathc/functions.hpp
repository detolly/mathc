#pragma once

#include <span>

#include <number.hpp>
#include <common.hpp>

namespace mathc
{

constexpr static inline execution_result sqrt(const std::span<number>)  { return number{0.0}; }
constexpr static inline execution_result log2(const std::span<number>)  { return number{0.0}; }
constexpr static inline execution_result ln  (const std::span<number>)  { return number{0.0}; }

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
