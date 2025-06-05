#include <string_view>
#include <string>
#include <utility>
#include <variant>
#include <memory>
#include <optional>

#pragma once

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

constexpr static inline auto operation_type_to_string(operation_type t)
{
    switch(t) {
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

using node = std::variant<op_node, constant_node, symbol_node>;

struct op_node
{
    std::unique_ptr<node> left;
    std::unique_ptr<node> right;
    operation_type type;
};

struct constant_node
{
    std::int64_t value;
};

struct symbol_node
{
    std::string value;
};

using parse_result = std::optional<node>;

template<typename T, typename... Args>
constexpr static inline auto make_parse_result(Args&&... args)
{
    return std::make_optional<node>(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
}

}
