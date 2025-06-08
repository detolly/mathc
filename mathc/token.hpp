#pragma once

#include <utility>
#include <string_view>

namespace mathc
{

using namespace std::string_view_literals;

enum class token_type
{
    null,
    op_mul,
    op_div,
    op_add,
    op_sub,
    op_exp,
    number_literal,
    alpha,
    paren_open,
    paren_close,
    comma
};

constexpr static bool token_type_is_operation(const token_type t)
{
    switch(t) {
        case token_type::op_mul:
        case token_type::op_div:
        case token_type::op_add:
        case token_type::op_sub:
        case token_type::op_exp:
            return true;
        case token_type::null:
        case token_type::number_literal:
        case token_type::alpha:
        case token_type::paren_open:
        case token_type::paren_close:
        case token_type::comma:
            return false;
    }

    std::unreachable();
}

constexpr std::string_view token_type_str(const token_type t)
{
    switch(t) {
        case token_type::null:              return "null"sv;
        case token_type::op_mul:            return "op_mul"sv;
        case token_type::op_div:            return "op_div"sv;
        case token_type::op_add:            return "op_add"sv;
        case token_type::op_sub:            return "op_sub"sv;
        case token_type::op_exp:            return "op_exp"sv;
        case token_type::number_literal:    return "number_literal"sv;
        case token_type::paren_open:        return "paren_open"sv;
        case token_type::paren_close:       return "paren_close"sv;
        case token_type::alpha:             return "alpha"sv;
        case token_type::comma:             return "comma"sv;
    }

    std::unreachable();
}

struct token
{
    token_type type{ token_type::null };
    std::string_view value{};
    bool has_decimal{ false };
    std::size_t index_in_stream{ 0 };
};

}
