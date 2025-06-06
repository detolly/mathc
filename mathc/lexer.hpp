#pragma once

#include <cassert>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

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
    paren_close
};

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
    }

    std::unreachable();
}

struct token
{
    token_type type{ token_type::null };
    std::string_view value{};
    bool has_decimal{ false };
};

constexpr static bool is_number(const char c)      { return c >= '0' && c <= '9'; }
constexpr static bool is_whitespace(const char c)  { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }
constexpr static bool is_operation(const char c)   { return c == '*' || c == '/' || c == '+' || c == '-' || c == '^'; }
constexpr static bool is_paren(const char c)       { return c == '(' || c == ')'; }

struct lexer
{
    std::string_view buffer{};
    std::vector<token> tokens{};
    std::size_t index{ 0 };

    constexpr inline auto can_consume() const { return index < buffer.length(); }
    constexpr inline auto current_iterator() const { return std::next(buffer.begin(), static_cast<long>(index)); }
    constexpr inline auto& current() const { return buffer[index]; }

    [[nodiscard]] constexpr inline bool consume() { index++; return can_consume(); }

    [[nodiscard]] constexpr inline bool consume_whitespace() {
        while(is_whitespace(current())) {
            if (!consume())
                return false;
        }
        return can_consume();
    }

    constexpr token parse_token();
    constexpr token parse_number_literal_token();
    constexpr token parse_operation_token();
    constexpr token parse_alpha_token();
    constexpr token parse_paren_token();

    constexpr void lex();

    constexpr static std::vector<token> lex(const std::string& buffer);
    constexpr static std::vector<token> lex(std::string_view buffer);

private:
    lexer() = default;
};

constexpr inline std::vector<token> lexer::lex(const std::string_view buffer)
{
    if (buffer.size() == 0)
        return {};

    lexer l;
    l.buffer = auto{ buffer };
    l.lex();
    return l.tokens;
}

constexpr inline std::vector<token> lexer::lex(const std::string& buffer)
{
    lexer l;
    l.buffer = std::string_view{ buffer };
    l.lex();
    return l.tokens;
}

constexpr inline void lexer::lex()
{
    while(true) {
        if (!can_consume())
            return;
        if (!consume_whitespace())
            return;

        tokens.emplace_back(parse_token());
    }
}

constexpr inline token lexer::parse_token()
{
    const auto current_char = current();

    if (is_number(current_char))            return parse_number_literal_token();
    else if (is_operation(current_char))    return parse_operation_token();
    else if (is_paren(current_char))        return parse_paren_token();
    else                                    return parse_alpha_token();
}

constexpr inline static token_type token_type_from_char(char c)
{
    switch(c) {
        case '*': return token_type::op_mul;
        case '/': return token_type::op_div;
        case '^': return token_type::op_exp;
        case '-': return token_type::op_sub;
        case '+': return token_type::op_add;
    }

    std::unreachable();
}

constexpr inline token lexer::parse_operation_token()
{
    const auto current = current_iterator();
    const auto _ = consume();

    return { token_type_from_char(*current), { current, 1 } };
}

constexpr inline token lexer::parse_alpha_token()
{
    const auto start = current_iterator();
    if (!consume())
        return { token_type::alpha, { start, current_iterator() } };

    do {
        const auto current_char = current();
        if (is_whitespace(current_char) ||
            is_paren(current_char) ||
            is_operation(current_char))
            return { token_type::alpha, { start, current_iterator() } };
    } while(consume());

    return { token_type::alpha, { start, current_iterator() } };
}

constexpr inline token lexer::parse_number_literal_token()
{
    const auto start = current_iterator();

    bool has_seen_decimal = false;
    while(is_number(current()) || (current() == '.' && !has_seen_decimal)) {
        if (current() == '.')
            has_seen_decimal = true;
        if (!consume())
            break;
    }

    return { token_type::number_literal, { start, current_iterator() }, has_seen_decimal };
}

constexpr inline token lexer::parse_paren_token()
{
    const auto current_token = current();
    const auto current = current_iterator();
    const auto _ = consume();

    switch(current_token) {
        case '(': return { token_type::paren_open, { current, 1 } };
        case ')': return { token_type::paren_close, { current, 1 } };
    }

    std::unreachable();
}

}
