#pragma once

#include <span>

#include <common.hpp>
#include <math.hpp>
#include <number.hpp>
#include <variant>

namespace mathc
{

struct function
{
    std::string_view name;
    std::optional<node>(&func)(const std::span<node> numbers);
};

template<typename T, typename... Args>
constexpr static inline std::optional<node> make_function_result(Args&& ...args)
{
    return std::make_optional<node>(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
}

constexpr static inline std::optional<node> vm_sqrt(const std::span<node> args)
{
    if (args.size() != 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};

    return make_function_result<constant_node>(math::sqrt(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static inline std::optional<node> vm_log2(const std::span<node> args)
{
    if (args.size() != 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};

    return make_function_result<constant_node>(math::log2(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static inline std::optional<node> vm_sum(const std::span<node> args)
{
    if (args.size() != 2)
        return {};

    const auto& amin = args[0];
    const auto& amax = args[1];
    if (!std::holds_alternative<constant_node>(amin) ||
        !std::holds_alternative<constant_node>(amax))
        return {};

    const auto& min = std::get<constant_node>(args[0]).value;
    const auto& max = std::get<constant_node>(args[1]).value;
    if (!min.is_int() || !max.is_int())
        return {};

    std::int64_t sum{ 0 };
    for(auto i = min.as_int(); i < max.as_int(); i++)
        sum += i;

    return make_function_result<constant_node>(number::from_int(sum));
}

constexpr static inline std::optional<node> vm_ln(const std::span<node> args)
{
    if (args.size() != 1)
        return {};

    const auto& arg = args[0];
    if (!std::holds_alternative<constant_node>(arg))
        return {};

    return make_function_result<constant_node>(math::log(std::get<constant_node>(arg).value.promote_to_double()));
}

constexpr static const auto functions =
{
    function{ "sqrt"sv,  vm_sqrt },
    function{ "log2"sv,  vm_log2 },
    function{ "ln"sv,    vm_ln   },
    function{ "sum"sv,   vm_sum   },
};

[[nodiscard]]
constexpr static inline std::optional<function> find_function(const std::string_view function_name)
{
    for(const auto& f : functions)
        if (f.name == function_name)
            return std::make_optional<function>(f);

    return {};
}

}
