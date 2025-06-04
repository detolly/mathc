#include <cassert>
#include <charconv>
#include <deque>
#include <initializer_list>
#include <optional>
#include <print>
#include <memory>
#include <source_location>
#include <string>
#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>
#include <expected>

using namespace std::string_view_literals;

template<class... Ts>
struct visitor_t : Ts... { using Ts::operator()...; };

namespace mathc
{

// ---------------------------------------- Lexer

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
    };

    std::unreachable();
}

struct token
{
    token_type type{ token_type::null };
    std::string_view value{};
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

    constexpr auto current_iterator() const { return buffer.begin() + static_cast<long>(index); };
    constexpr auto& current() const { return buffer[index]; }

    [[nodiscard]] constexpr bool consume() { return ++index < buffer.length(); };
    [[nodiscard]] constexpr bool consume_while_number()
    { 
        bool has_seen_decimal = false;
        while(is_number(current()) || (current() == '.' && !has_seen_decimal)) {
            if (current() == '.')
                has_seen_decimal = true;
            if (!consume())
                return false;
        }
        return true;
    };

    [[nodiscard]] constexpr bool consume_whitespace() {
        while(is_whitespace(current())) {
            if (!consume())
                return false;
        }
        return true;
    }

    constexpr std::optional<token> parse_token();
    constexpr std::optional<token> parse_number_literal_token();
    constexpr std::optional<token> parse_operation_token();
    constexpr std::optional<token> parse_alpha_token();
    constexpr std::optional<token> parse_paren_token();

    constexpr void lex();

    constexpr static std::optional<std::vector<token>> lex(const std::string& buffer);
    constexpr static std::optional<std::vector<token>> lex(std::string_view buffer);

private:
    lexer() = default;
};

constexpr std::optional<std::vector<token>> lexer::lex(const std::string_view buffer)
{
    lexer l;
    l.buffer = buffer;
    l.lex();
    return l.tokens;
}

constexpr std::optional<std::vector<token>> lexer::lex(const std::string& buffer)
{
    lexer l;
    l.buffer = buffer;
    l.lex();
    return l.tokens;
}

constexpr void lexer::lex()
{
    while(true) {
        const auto t = parse_token();
        if (!t.has_value())
            break;
        tokens.push_back(t.value());
    }
}

constexpr std::optional<token> lexer::parse_token()
{
    if (index >= buffer.length())
        return {};

    if (!consume_whitespace())
        return {};

    const auto current_char = current();

    if (is_number(current_char))
        return parse_number_literal_token();
    else if (is_operation(current_char))
        return parse_operation_token();
    else if (is_paren(current_char))
        return parse_paren_token();
    else
        return parse_alpha_token();
};

constexpr std::optional<token> lexer::parse_operation_token()
{
    const auto current = current_iterator();
    const auto _ = consume();

    switch(*current) {
        case '*': return std::optional<token> {{ .type=token_type::op_mul, .value={ current, current + 1 } }};
        case '/': return std::optional<token> {{ .type=token_type::op_div, .value={ current, current + 1 } }};
        case '^': return std::optional<token> {{ .type=token_type::op_exp, .value={ current, current + 1 } }};
        case '-': return std::optional<token> {{ .type=token_type::op_add, .value={ current, current + 1 } }};
        case '+': return std::optional<token> {{ .type=token_type::op_add, .value={ current, current + 1 } }};
    }

    throw std::runtime_error(std::format("Operation not recognized: {}", *current));
}

constexpr std::optional<token> lexer::parse_alpha_token()
{
    const auto start = current_iterator();
    if (!consume())
        return std::optional<token>{{ .type=token_type::alpha, .value={ start, current_iterator() } }};

    while(true) {
        const auto current_char = current();
        if (is_whitespace(current_char) ||
            is_paren(current_char) ||
            is_operation(current_char))
            return std::optional<token>{{ .type=token_type::alpha, .value={ start, current_iterator() } }};
        if (!consume())
            return std::optional<token>{{ .type=token_type::alpha, .value={ start, current_iterator() }}};
    }

    std::unreachable();
}

constexpr std::optional<token> lexer::parse_number_literal_token()
{
    const auto start = current_iterator();
    const auto _ = consume_while_number();

    return std::optional<token>{{ .type=token_type::number_literal, .value={ start, current_iterator() } }};
}

constexpr std::optional<token> lexer::parse_paren_token()
{
    const auto current_token = current();
    const auto current = current_iterator();
    const auto _ = consume();

    switch(current_token) {
        case '(': return std::optional<token>{{ .type=token_type::paren_open, .value={ current, current + 1 } }};
        case ')': return std::optional<token>{{ .type=token_type::paren_close, .value={ current, current + 1 } }};
    }

    return {};
}

// ---------------------------------------- Parser

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
    op_node(std::unique_ptr<node>&& l, std::unique_ptr<node>&& r, operation_type t);
    std::string format() const;
    std::unique_ptr<node> left;
    std::unique_ptr<node> right;
    operation_type type;
};

struct constant_node
{
    constant_node(int c) : value(c) {}
    std::string format() const { return std::format("constant_node: {}", value); }
    int value;
};

struct symbol_node
{
    template<typename S>
    symbol_node(const S& str) : value(str) {}
    std::string format() const { return std::format("symbol_node: {}", value); }
    std::string value;
};

op_node::op_node(std::unique_ptr<node>&& l, std::unique_ptr<node>&& r, operation_type t)
    : left(std::move(l)), right(std::move(r)), type(t)
{}

std::string op_node::format() const
{
    return std::format("op_node: \n\t{}\n\t{}\n\t{}",
                       left->visit(visitor_t{ [](const auto& node){ return node.format(); } }),
                       operation_type_to_string(type),
                       right->visit(visitor_t{ [](const auto& node){ return node.format(); } }));
}

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
    [[nodiscard]] constexpr bool consume(const std::source_location& sl = std::source_location::current())
    {
        std::println(stderr, "{}: consumed {}", sl.function_name(), tokens[index].value);
        return index++ < tokens.size();
    }

    constexpr std::optional<std::unique_ptr<node>> parse();
    constexpr std::optional<std::unique_ptr<node>> parse_expression();
    constexpr std::optional<std::unique_ptr<node>> parse_term();
    constexpr std::optional<std::unique_ptr<node>> parse_factor();
    constexpr std::optional<std::unique_ptr<node>> parse_var();

    constexpr std::optional<std::unique_ptr<node>> parse_constant();
    constexpr std::optional<std::unique_ptr<node>> parse_symbol();

    constexpr static std::optional<std::unique_ptr<node>> parse(const std::span<const token> tokens);

private:
    parser() = default;
};

constexpr std::optional<std::unique_ptr<node>> parser::parse()
{
    return parse_expression();
}

// grammar:
//
// <expr> ::= <term> '+' <expr> | <term> '-' <expr> | <term>
// <term> ::= <factor> '*' <term> | <factor> '/' <term> | <factor>
// <factor> ::= <var> ^ <factor> | <var>
// <var> ::= <constant> | <symbol> | '(' <expr> ')'

constexpr std::optional<std::unique_ptr<node>> parser::parse_expression()
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
        return term;

    const auto op_type = op_token.type == token_type::op_add ? operation_type::add : operation_type::sub;
    return std::optional<std::unique_ptr<node>> {
        std::make_unique<node>(
            std::in_place_type_t<op_node>{},
            std::move(term.value()),
            std::move(expr.value()),
            op_type
        )
    };
}

constexpr std::optional<std::unique_ptr<node>> parser::parse_term()
{
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
        return factor;

    const auto op_type = op_token.type == token_type::op_mul ? operation_type::mul : operation_type::div;
    return std::optional<std::unique_ptr<node>> {
        std::make_unique<node>(
            std::in_place_type_t<op_node>{},
            std::move(factor.value()),
            std::move(term.value()),
            op_type
        )
    };
}

constexpr std::optional<std::unique_ptr<node>> parser::parse_factor()
{
    auto var = parse_var();
    if (!var.has_value())
        return {};

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

    return std::optional<std::unique_ptr<node>> {
        std::make_unique<node>(
            std::in_place_type_t<op_node>{},
            std::move(var.value()),
            std::move(factor.value()),
            operation_type::exp
        )
    };
}

constexpr std::optional<std::unique_ptr<node>> parser::parse_var()
{
    auto constant = parse_constant();
    if (constant.has_value())
        return constant;

    auto symbol = parse_symbol();
    if (symbol.has_value())
        return symbol;

    const auto paren_open_token_or = current(); 
    if (!paren_open_token_or.has_value())
        return {};

    const auto& paren_open_token = paren_open_token_or.value();
    if (paren_open_token.type != token_type::paren_open)
        throw std::runtime_error("expected expression");

    assert(consume());
    auto expr = parse_expression();

    const auto paren_close_token_or = current(); 
    if (!paren_close_token_or.has_value())
        return {};

    const auto& paren_close_token = paren_close_token_or.value();
    if (paren_close_token.type != token_type::paren_close)
        throw std::runtime_error("expected )");

    assert(consume());

    return expr;
}

constexpr std::optional<std::unique_ptr<node>> parser::parse_symbol()
{
    const auto& current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::alpha)
        return {};

    assert(consume());

    return std::optional<std::unique_ptr<node>> {
        std::make_unique<node>(
            std::in_place_type_t<symbol_node>{},
            current_token.value
        )
    };
}

constexpr std::optional<std::unique_ptr<node>> parser::parse_constant()
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

    return std::optional<std::unique_ptr<node>> {
        std::make_unique<node>(
            std::in_place_type_t<constant_node>{},
            val
        )
    };
}

constexpr std::optional<std::unique_ptr<node>> parser::parse(const std::span<const token> tokens)
{
    parser p;
    p.tokens = tokens;
    return p.parse();
}

}

using namespace mathc;

int main(int, const char* argv[])
{
    const auto tokens = mathc::lexer::lex(std::string_view(argv[1]));
    if (!tokens.has_value())
        return 1;

    for(const auto& t : tokens.value())
        std::println("{} {}", mathc::token_type_str(t.type), t.value);

    const auto root_node = mathc::parser::parse(std::span{ tokens.value().begin(), tokens.value().end() });
    if (!root_node.has_value())
        return 1;

    auto visitor = visitor_t{
        [](const auto& st){ std::println("{}", st.format()); }
    };
    
    root_node.value()->visit(visitor);
}
