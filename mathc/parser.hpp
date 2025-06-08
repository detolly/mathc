#pragma once

#include <cassert>
#include <memory>
#include <optional>
#include <utility>

#include <common.hpp>
#include <functions.hpp>
#include <node.hpp>
#include <token.hpp>

namespace mathc
{

struct parse_error
{
    token token;
    std::string error;
};

using parse_result = std::expected<std::optional<node>, parse_error>;

template<typename T, typename... Args>
constexpr static inline parse_result make_parse_result(Args&&... args)
{
    return parse_result{ std::in_place_t{},
                         std::in_place_t{},
                         std::in_place_type_t<T>{},
                         std::forward<Args>(args)... };
}

constexpr static inline auto make_parse_result()
{
    return parse_result{ std::in_place_t{} };
}

struct parser
{
    std::span<const token> tokens{};
    std::size_t index{ 0 };

    template<typename... Args>
    constexpr inline auto make_parse_error(Args&&... args)
    {
        if (current().has_value())
            return parse_result{ std::unexpect_t{}, current().value(), std::forward<Args>(args)... };

        return parse_result{ std::unexpect_t{}, *tokens.rbegin(), std::forward<Args>(args)... };
    }

    [[nodiscard]] constexpr inline const std::optional<token> current() const
    {
        if (index < tokens.size())
            return std::make_optional(tokens[index]);

        return {};
    }
    [[nodiscard]] constexpr inline const std::optional<token> peek() const
    {
        if (index + 1 >= tokens.size())
            return {};

        return std::make_optional(tokens[index + 1]);
    }
    [[nodiscard]] constexpr inline bool consume() { return index++ < tokens.size(); }

    constexpr parse_result parse();
    constexpr parse_result parse_expression();
    constexpr parse_result parse_term();
    constexpr parse_result parse_factor();
    constexpr parse_result parse_var();

    constexpr parse_result parse_constant();
    constexpr parse_result parse_symbol();
    constexpr parse_result parse_function_call(const std::string_view function_name);

    constexpr parse_result parse_paren_expression();
    constexpr parse_result parse_multiplication_paren_expression(node&& value);

    constexpr static parse_result parse(const std::span<const token> tokens);

private:
    parser() = default;
};

// Implementation

constexpr inline parse_result parser::parse()
{
    return parse_expression();
}

// grammar:
//
// <expr> ::= <term> <expr> | <term> | <end of stream>
// <term> ::= <factor> '*' <term> | <factor> '/' <term> | <factor>
// <factor> ::= <var> ^ <factor> | <var>
// <var> ::= [-+]? (<constant> '(' <expr> ')' | <symbol> | <symbol_function_call> | '(' <expr> ')' | <constant> | <symbol>)
// <function_call> ::= <symbol> '(' ( <expr> (,)? ){0,} ')' 
// <constant> ::= [0-9]+(\.[0-9]{1,})?
// <symbol> ::= [A-Za-z]+

constexpr inline parse_result parser::parse_expression()
{
    TRY(term_or, parse_term());
    if (!term_or.has_value())
        return term_or_error; 

    auto& term = term_or.value();
    TRY(expr_or, parse_expression());
    if (!expr_or.has_value())
        return term_or_error;

    auto& expr = expr_or.value();
    return make_parse_result<op_node>(std::make_unique<node>(std::move(term)),
                                      std::make_unique<node>(std::move(expr)),
                                      operation_type::add);
}

constexpr inline parse_result parser::parse_term()
{
    TRY(factor_or, parse_factor());
    if (!factor_or.has_value())
        return factor_or_error;

    auto& factor = factor_or.value();

    const auto op_token_or = current(); 
    if (!op_token_or.has_value())
        return factor_or_error;

    const auto& op_token = op_token_or.value();
    if (op_token.type != token_type::op_mul &&
        op_token.type != token_type::op_div)
        return factor_or_error;

    assert(consume());

    TRY(term_or, parse_term());
    if (!term_or.has_value())
        return make_parse_error("Expected term.");

    auto& term = term_or.value();

    const auto op_type = op_token.type == token_type::op_mul ? operation_type::mul : operation_type::div;
    return make_parse_result<op_node>(std::make_unique<node>(std::move(factor)),
                                      std::make_unique<node>(std::move(term)),
                                      op_type);
}

constexpr inline parse_result parser::parse_factor()
{
    TRY(var_or, parse_var());
    if (!var_or.has_value())
        return var_or_error;

    auto& var = var_or.value();

    const auto op_token_or = current(); 
    if (!op_token_or.has_value())
        return var_or_error;

    const auto& op_token = op_token_or.value();
    if (op_token.type != token_type::op_exp)
        return var_or_error;

    assert(consume());

    TRY(factor_or, parse_factor());
    if (!factor_or.has_value())
        return make_parse_error("Expected factor.");

    auto& factor = factor_or.value();
    return make_parse_result<op_node>(std::make_unique<node>(std::move(var)),
                                      std::make_unique<node>(std::move(factor)),
                                      operation_type::exp);
}

constexpr inline parse_result parser::parse_paren_expression()
{
    const auto paren_open_token_or = current(); 
    if (!paren_open_token_or.has_value())
        return make_parse_result();

    const auto& paren_open_token = paren_open_token_or.value();
    if (paren_open_token.type != token_type::paren_open)
        return make_parse_result();

    assert(consume());

    TRY(expr_or, parse_expression());
    if (!expr_or.has_value())
        return make_parse_error("Expected expression");

    const auto paren_close_token_or = current(); 
    if (!paren_close_token_or.has_value())
        return make_parse_error("Unexpected end of stream, expected )");

    const auto& paren_close_token = paren_close_token_or.value();
    if (paren_close_token.type != token_type::paren_close)
        return make_parse_error(std::format("Expected ), got {}", paren_close_token.value));

    assert(consume());

    return expr_or_error;
}

constexpr inline parse_result parser::parse_multiplication_paren_expression(node&& value)
{
    TRY(expr_or, parse_paren_expression());
    if (!expr_or.has_value())
        return make_parse_result();

    auto& expr = expr_or.value();
    return make_parse_result<op_node>(std::make_unique<node>(std::move(value)),
                                      std::make_unique<node>(std::move(expr)),
                                      operation_type::mul);
}

constexpr inline parse_result parser::parse_var()
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return make_parse_result();

    const auto& current_token = current_token_or.value();
    if (current_token.type == token_type::op_sub) {
        assert(consume());

        TRY(var_or, parse_var());
        if (!var_or.has_value())
            return make_parse_error("Expected var, got nothing.");

        auto& var = var_or.value();
        return make_parse_result<op_node>(std::make_unique<node>(std::in_place_type_t<constant_node>{},
                                                                 number::from_int(-1)),
                                          std::make_unique<node>(std::move(var)),
                                          operation_type::mul);
    }

    if (current_token.type == token_type::op_add) {
        assert(consume());

        TRY(var_or, parse_var());
        if (!var_or.has_value())
            return make_parse_error("Expected term.");

        return var_or_error;
    }

    TRY(constant_or, parse_constant());
    if (constant_or.has_value()) {
        auto& constant = constant_or.value();
        TRY(paren_multiplication_or, parse_multiplication_paren_expression(std::move(constant)));
        if (paren_multiplication_or.has_value())
            return paren_multiplication_or_error;

        const auto op_token_or = current();
        if (!op_token_or.has_value())
            return constant_or_error;

        const auto& op_token = op_token_or.value();
        if (token_type_is_operation(op_token.type))
            return constant_or_error;

        TRY(factor_or, parse_factor());
        if (factor_or.has_value())
            return make_parse_result<op_node>(std::make_unique<node>(std::move(constant_or.value())),
                                              std::make_unique<node>(std::move(factor_or.value())),
                                              operation_type::mul);
        return constant_or_error;
    }

    TRY(symbol_or, parse_symbol());
    if (symbol_or.has_value()) {
        auto& symbol = symbol_or.value();
        auto& node = std::get<symbol_node>(symbol);

        const auto* function = find_function(node.value);
        if (function != nullptr) {
            TRY(function_call_or, parse_function_call(node.value));
            if (function_call_or.has_value())
                return function_call_or_error;
        }

        TRY(paren_multiplication_or, parse_multiplication_paren_expression(std::move(symbol)));
        if (!paren_multiplication_or.has_value())
            return symbol_or_error;

        return paren_multiplication_or_error;
    }

    return parse_paren_expression();
}

constexpr inline parse_result parser::parse_function_call(const std::string_view function_name)
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return make_parse_result();

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::paren_open)
        return make_parse_result();

    assert(consume());
    function_call_node node{ std::string{ function_name }, {} };

    while(true) {
        TRY(expr_or, parse_expression());
        if (!expr_or.has_value()) {
            const auto stop_token_or = current();
            if (!stop_token_or.has_value())
                return make_parse_error("Unexpected end of stream. Missing ).");

            const auto& stop_token = stop_token_or.value();
            if (stop_token.type != token_type::paren_close)
                return make_parse_error("Expected end of function, got junk.");

            assert(consume());
            break;
        }

        node.arguments.emplace_back(std::move(expr_or.value()));

        const auto comma_token_or = current();
        if (!comma_token_or.has_value())
            return make_parse_error("Unexpected end of stream. Missing ).");

        const auto& comma_token = comma_token_or.value();
        if (comma_token.type == token_type::comma)
            assert(consume());
        else if (comma_token.type == token_type::paren_close) {
            assert(consume());
            break;
        } else
            return make_parse_error("Junk encountered while parsing function arguments.");
    }

    return make_parse_result<function_call_node>(std::move(node));
}

constexpr inline parse_result parser::parse_symbol()
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return make_parse_result();

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::alpha)
        return make_parse_result();

    assert(consume());

    return make_parse_result<symbol_node>(std::string{ current_token.value });
}

constexpr inline parse_result parser::parse_constant()
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return make_parse_result();

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::number_literal)
        return make_parse_result();

    auto number = number::from_token(current_token);
    if (!number.has_value())
        return make_parse_error(std::format("Invalid number: {}", current_token.value));

    assert(consume());

    return make_parse_result<constant_node>(std::move(number.value()));
}

constexpr inline parse_result parser::parse(const std::span<const token> tokens)
{
    if (tokens.size() == 0)
        return make_parse_result();

    parser p;
    p.tokens = tokens;
    return p.parse();
}

}
