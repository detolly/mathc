#pragma once

#include <cassert>
#include <charconv>
#include <optional>
#include <memory>
#include <source_location>
#include <string>
#include <utility>
#include <variant>

#include <lexer.hpp>

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

constexpr auto operation_type_to_string(operation_type t)
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
    int value;
};

struct symbol_node
{
    std::string value;
};

using parse_result = std::optional<node>;

struct parser
{
    std::span<const token> tokens{};
    std::size_t index{ 0 };

    [[nodiscard]] constexpr const std::optional<token> current() const
    {
        if (index < tokens.size())
            return std::optional{ tokens[index] };

        return {};
    }
    [[nodiscard]] constexpr const std::optional<token> peek() const
    {
        if (index + 1 >= tokens.size())
            return {};

        return tokens[index + 1];
    }
    [[nodiscard]] constexpr bool consume(
        // const std::source_location& sl = std::source_location::current()
    )
    {
        // std::println(stderr, "{}: consumed {}", sl.function_name(), tokens[index].value);
        return index++ < tokens.size();
    }

    constexpr parse_result parse();
    constexpr parse_result parse_expression();
    constexpr parse_result parse_term();
    constexpr parse_result parse_factor();
    constexpr parse_result parse_var();

    constexpr parse_result parse_constant();
    constexpr parse_result parse_symbol();

    constexpr parse_result parse_paren_expression();
    constexpr parse_result parse_multiplication_paren_expression(auto&& value);

    constexpr static parse_result parse(const std::span<const token> tokens);

private:
    parser() = default;
};

constexpr parse_result parser::parse()
{
    return parse_expression();
}

template<typename T, typename... Args>
constexpr static auto make_parse_result(Args&&... args)
{
    return std::make_optional<node>(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
}

// grammar:
//
// <expr> ::= <term> '+' <expr> | <term> '-' <expr> | <term>
// <term> ::= [-+]? <factor> '*' <term> | [-+]? <factor> '/' <term> | [-+]? <factor>
// <factor> ::= <var> ^ <factor> | <var>
// <var> ::= <constant> '(' <expr> ')' | <symbol> '(' <expr> ')' | '(' <expr> ')' | <constant> | <symbol>
// <constant> ::= [0-9]+(\.[0-9]{1,})?
// <symbol> ::= [A-Za-z]+

constexpr parse_result parser::parse_expression()
{
    auto term = parse_term();
    if (!term.has_value())
        return {};

    const auto op_token_or = current(); 
    if (!op_token_or.has_value())
        return term;

    const auto& op_token = op_token_or.value();
    if (op_token.type != token_type::op_add &&
        op_token.type != token_type::op_sub)
        return term;

    assert(consume());

    auto expr = parse_expression();
    if (!expr.has_value())
        throw std::runtime_error("expected expression");

    const auto op_type = op_token.type == token_type::op_add ? operation_type::add : operation_type::sub;
    return make_parse_result<op_node>(std::make_unique<node>(std::move(term.value())),
                                      std::make_unique<node>(std::move(expr.value())),
                                      op_type);
}

constexpr parse_result parser::parse_term()
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type == token_type::op_sub) {
        assert(consume());
        auto ret = parse_term();
        if (!ret.has_value())
            throw std::runtime_error("expected term");

        return make_parse_result<op_node>(std::make_unique<node>(constant_node{ 0 }),
                                          std::make_unique<node>(std::move(ret.value())),
                                          operation_type::sub);
    }

    if (current_token.type == token_type::op_add) {
        assert(consume());
        auto term = parse_term();
        if (!term.has_value())
            throw std::runtime_error("expected term");

        return term;
    }

    auto factor = parse_factor();
    if (!factor.has_value())
        return {};

    const auto op_token_or = current(); 
    if (!op_token_or.has_value())
        return factor;

    const auto& op_token = op_token_or.value();
    if (op_token.type != token_type::op_mul &&
        op_token.type != token_type::op_div)
        return factor;

    assert(consume());

    auto term = parse_term();
    if (!term.has_value())
        throw std::runtime_error("Expected term");

    const auto op_type = op_token.type == token_type::op_mul ? operation_type::mul : operation_type::div;
    return make_parse_result<op_node>(std::make_unique<node>(std::move(factor.value())),
                                      std::make_unique<node>(std::move(term.value())),
                                      op_type);
}

constexpr parse_result parser::parse_factor()
{
    auto var = parse_var();
    if (!var.has_value())
        return var;

    const auto op_token_or = current(); 
    if (!op_token_or.has_value())
        return var;

    const auto& op_token = op_token_or.value();
    if (op_token.type != token_type::op_exp)
        return var;

    assert(consume());

    auto factor = parse_factor();
    if (!factor.has_value())
        return var;

    return make_parse_result<op_node>(std::make_unique<node>(std::move(var.value())),
                                      std::make_unique<node>(std::move(factor.value())),
                                      operation_type::exp);
}

// <var> ::= <constant> '(' <expr> ')' | <symbol> '(' <expr> ')' | <constant> | <symbol> | '(' <expr> ')' 

constexpr parse_result parser::parse_paren_expression()
{
    const auto paren_open_token_or = current(); 
    if (!paren_open_token_or.has_value())
        return {};

    const auto& paren_open_token = paren_open_token_or.value();
    if (paren_open_token.type != token_type::paren_open)
        return {};

    assert(consume());
    auto expr = parse_expression();

    const auto paren_close_token_or = current(); 
    if (!paren_close_token_or.has_value())
        throw std::runtime_error("unexpected end of stream, expected )");

    const auto& paren_close_token = paren_close_token_or.value();
    if (paren_close_token.type != token_type::paren_close)
        throw std::runtime_error("expected )");

    assert(consume());

    return expr;
}

constexpr parse_result parser::parse_multiplication_paren_expression(auto&& value)
{
    auto expr = parse_paren_expression();
    if (!expr.has_value())
        return {};

    return make_parse_result<op_node>(std::make_unique<node>(std::move(value.value())),
                                      std::make_unique<node>(std::move(expr.value())),
                                      operation_type::mul);
}

constexpr parse_result parser::parse_var()
{
    auto constant = parse_constant();
    if (constant.has_value()) {
        auto paren_multiplication = parse_multiplication_paren_expression(std::move(constant));
        if (!paren_multiplication.has_value())
            return constant;

        return paren_multiplication;
    }

    auto symbol = parse_symbol();
    if (symbol.has_value()) {
        auto paren_multiplication = parse_multiplication_paren_expression(std::move(constant));
        if (!paren_multiplication.has_value())
            return symbol;

        return paren_multiplication;
    }

    return parse_paren_expression();
}

constexpr parse_result parser::parse_symbol()
{
    const auto& current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::alpha)
        return {};

    assert(consume());

    return make_parse_result<symbol_node>(std::string(current_token.value));
}

constexpr parse_result parser::parse_constant()
{
    const auto& current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::number_literal)
        return {};

    int val;
    const auto res = std::from_chars(current_token.value.begin(), current_token.value.end(), val);
    assert(res.ec == std::errc{});
    assert(consume());

    return make_parse_result<constant_node>(val);
}

constexpr parse_result parser::parse(const std::span<const token> tokens)
{
    if (tokens.size() == 0)
        return {};

    parser p;
    p.tokens = tokens;
    return p.parse();
}

}
