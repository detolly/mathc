#pragma once

#include <expected>
#include <string>

#include <node.hpp>
#include <number.hpp>
#include <token.hpp>

namespace mathc
{

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-macros"

#define PROPAGATE_ERROR(x, expression)                      \
        auto&& x ## _result = (expression);                 \
        if (!x ## _result.has_value()) [[unlikely]]         \
            return x ## _result;                            \
        [[maybe_unused]] auto&& x = x ## _result .value()   \

#pragma GCC diagnostic pop

struct execution_error
{
    std::string error;
};

using simplify_result = std::variant<number, node>;
using execution_result = std::expected<simplify_result, execution_error>;

template<typename T, typename... Args>
    requires(node_type<T>)
constexpr static execution_result make_execution_result(Args&&... args)
{
    return execution_result{ std::in_place_t{},
                             std::in_place_type_t<node>{},
                             std::in_place_type_t<T>{},
                             std::forward<Args>(args)... };
}

template<typename T, typename... Args>
    requires(!node_type<T>)
constexpr static execution_result make_execution_result(Args&&... args)
{
    return execution_result{ std::in_place_t{},
                             std::in_place_type_t<T>{},
                             std::forward<Args>(args)... };
}

template<typename... Args>
constexpr static execution_result make_execution_error(Args&&... args)
{
    return execution_result{ std::unexpect_t{},
                             std::forward<Args>(args)... };
}

struct function
{
    std::string_view name;
    execution_result(&func)(const std::span<number> numbers);
};


}
