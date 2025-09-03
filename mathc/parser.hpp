#pragma once

#include <cassert>
#include <expected>
#include <memory>
#include <optional>
#include <utility>

#include <common.hpp>
#include <node.hpp>
#include <token.hpp>

namespace mathc
{

struct parse_error
{
    token token;
    std::string error;
};

using parse_result = std::expected<node, parse_error>;

template<typename T, typename... Args>
constexpr static inline parse_result make_parse_result_node(Args&&... args)
{
    return parse_result{ std::in_place_t{},
                         std::in_place_type_t<T>{},
                         std::forward<Args>(args)... };
}

struct [[nodiscard]]  parser
{
    std::span<const token> tokens{};
    std::size_t index{ 0 };

    template<typename... Args>
    constexpr inline auto make_parse_error(Args&&... args) const
    {
        const auto current_token = current();
        if (current_token.has_value())
            return parse_result{ std::unexpect_t{},
                                 current_token.value(),
                                 std::forward<Args>(args)... };

        return parse_result{ std::unexpect_t{},
                             *tokens.rbegin(),
                             std::forward<Args>(args)... };
    }

    constexpr inline bool current_is(token_type t) const
    {
        return index < tokens.size() ? tokens[index].type == t : false;
    }

    constexpr inline const std::optional<std::reference_wrapper<const token>> current() const
    {
        return index < tokens.size() ? 
                   std::make_optional(std::ref(tokens[index])) :
                   std::optional<std::reference_wrapper<const token>>{};
    }

    constexpr inline const std::optional<std::reference_wrapper<const token>> peek() const
    {
        return index + 1 < tokens.size() ?
                   std::make_optional(std::ref(tokens[index + 1])) :
                   std::optional<std::reference_wrapper<const token>>{};
    }

    constexpr inline bool consume() { return (index++) < tokens.size(); }

    constexpr parse_result parse();
    constexpr parse_result parse_expression();
    constexpr parse_result parse_term();
    constexpr parse_result parse_factor();
    constexpr parse_result parse_var();

    constexpr parse_result parse_constant();
    constexpr parse_result parse_symbol();
    constexpr parse_result parse_function_call(const std::string_view function_name);

    constexpr parse_result parse_paren_expression();

    constexpr static parse_result parse(const std::span<const token> tokens);

    template<token_type t, token_type... ts>
    constexpr inline std::tuple<bool, token_type> current_token_is()
    {
        const auto token_or = current();
        if (!token_or.has_value())
            return { false, token_type::null };

        const auto matches = token_or.value().get().type == t;
        if (matches)
            return { true, t };

        if constexpr (sizeof...(ts) == 0)
            return { false, token_type::null };
        else
            return current_token_is<ts...>();
    }

private:
    parser() = default;
};

// Implementation

constexpr inline parse_result parser::parse()
{
    return parse_expression();
}

// grammar:
// <constant> is builtin
// <symbol> is builtin
//
// <expr> = <term> { ('+'|'-') <term> }
// <term> = <factor> { ('*'|'/') <factor> } [<implicit_multiplication>]
// <implicit_multiplication> = [<number> | <paren_expression> | <symbol>]:<factor>
// <factor> = ['+'|'-'] <var> { ^ <var> }
// <var> = <constant> | <symbol> [<function_call>] | <paren_expression>
// <paren_expression> = '(' <expr> ')'
// <function_call> = '(' <expr> { ',' <expr> } ')' 

constexpr inline parse_result parser::parse_expression()
{
    PROPAGATE_ERROR(term, parse_term());

    while(true) {
        const auto [found, type] = current_token_is<token_type::op_add, token_type::op_sub>();
        if (!found) break;

        assert(consume());
        PROPAGATE_ERROR(term2, parse_term());
        term = make_node<op_node>(std::make_unique<node>(std::move(term)),
                                  std::make_unique<node>(std::move(term2)),
                                  (type == token_type::op_sub ? operation_type::sub : operation_type::add));
    }

    return term_result;
}

constexpr inline parse_result parser::parse_term()
{
    PROPAGATE_ERROR(factor, parse_factor());

    while(true) {
        const auto [found, type] = current_token_is<token_type::op_mul,
                                                    token_type::op_div>();
        if (!found) break;

        assert(consume());
        PROPAGATE_ERROR(factor2, parse_factor());
        factor = make_node<op_node>(std::make_unique<node>(std::move(factor)),
                                    std::make_unique<node>(std::move(factor2)),
                                    (type == token_type::op_mul ? operation_type::mul : operation_type::div));
    }

    while(true) {
        const auto [implicit_multiplication_found, _] = current_token_is<token_type::number_literal,
                                                                         token_type::paren_open,
                                                                         token_type::alpha>();
        if (!implicit_multiplication_found) break;

        PROPAGATE_ERROR(factor2, parse_factor());
        factor = make_node<op_node>(std::make_unique<node>(std::move(factor)),
                                    std::make_unique<node>(std::move(factor2)),
                                    operation_type::mul);
    }

    return factor_result;
}

constexpr inline parse_result parser::parse_factor()
{
    bool negate{ false };
    if (const auto [found, type] = current_token_is<token_type::op_add, token_type::op_sub>(); found) {
        negate = (type == token_type::op_sub);
        assert(consume());
    }

    PROPAGATE_ERROR(var, parse_var());

    if (negate)
        var = make_node<op_node>(std::make_unique<node>(std::move(var)),
                                 make_unique_node<constant_node>(number::from_int(-1)),
                                 operation_type::mul);

    while (true) {
        const auto [found, _] = current_token_is<token_type::op_exp>();
        if (!found) break;

        assert(consume());
        PROPAGATE_ERROR(var2, parse_var());
        var = make_node<op_node>(std::make_unique<node>(std::move(var)),
                                 std::make_unique<node>(std::move(var2)),
                                 operation_type::exp);
    }

    return var_result;
}

constexpr inline parse_result parser::parse_paren_expression()
{
    if (const auto [found, _] = current_token_is<token_type::paren_open>(); !found)
        return make_parse_error("Expected (.");

    assert(consume());
    PROPAGATE_ERROR(expr, parse_expression());
    if (const auto [found, _] = current_token_is<token_type::paren_close>(); !found)
        return make_parse_error("Expected ).");

    assert(consume());
    return expr_result;
}

constexpr inline parse_result parser::parse_var()
{
    if (const auto [constant_found, _] = current_token_is<token_type::number_literal>(); constant_found) {
        PROPAGATE_ERROR(constant, parse_constant());
        return constant_result;
    }

    if (const auto [symbol_found, _] = current_token_is<token_type::alpha>(); symbol_found) {
        PROPAGATE_ERROR(symbol, parse_symbol());
        const auto& value = std::get<symbol_node>(symbol).value;

        if (math::has_function(value)) {
            PROPAGATE_ERROR(function_call, parse_function_call(value));
            return function_call_result;
        }

        return symbol_result;
    }

    if (const auto [paren_found, _] = current_token_is<token_type::paren_open>(); paren_found) {
        PROPAGATE_ERROR(expr, parse_paren_expression());
        return expr_result;
    }

    return make_parse_error("Unexpected token.");
}

constexpr inline parse_result parser::parse_function_call(const std::string_view function_name)
{
    if (const auto [paren_found, _] = current_token_is<token_type::paren_open>(); !paren_found)
        return make_parse_error("Expected function call.");

    assert(consume());

    auto result = make_parse_result_node<function_call_node>(std::string{ function_name }, std::vector<node>{});
    auto& node = std::get<function_call_node>(result.value());

    while(true) {
        PROPAGATE_ERROR(expr, parse_expression());
        node.arguments.emplace_back(std::move(expr));

        if (const auto [close_token, type] =
            current_token_is<token_type::comma, token_type::paren_close>(); close_token) {
            if (type == token_type::comma) {
                assert(consume());
                continue;
            } else if (type == token_type::paren_close) {
                assert(consume());
                break;
            }
        }

        return make_parse_error("Junk encountered while parsing function arguments.");
    }

    return result;
}

constexpr inline parse_result parser::parse_symbol()
{
    if (const auto [is_symbol, _] = current_token_is<token_type::alpha>(); !is_symbol)
        return make_parse_error("Invalid symbol encountered.");

    const auto& token = current().value().get();
    assert(consume());

    return make_parse_result_node<symbol_node>(token.value);
}

constexpr inline parse_result parser::parse_constant()
{
    if (const auto [is_number, _] = current_token_is<token_type::number_literal>(); !is_number)
        return make_parse_error("Not a number.");

    auto number = number::from_token(current().value().get());
    if (!number.has_value())
        return make_parse_error(std::format("Invalid number: {}", current().value().get().value));

    assert(consume());
    return make_parse_result_node<constant_node>(std::move(number.value()));
}

constexpr inline parse_result parser::parse(const std::span<const token> tokens)
{
    if (tokens.size() == 0)
        return parse_result{ std::unexpect_t{}, token{}, "Expected expression",  };

    parser p;
    p.tokens = tokens;
    return p.parse();
}

}
