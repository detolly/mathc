#include <cassert>
#include <charconv>
#include <cmath>
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
    if (buffer.size() == 0)
        return {};

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
        case '*': return std::optional<token> { token{ .type=token_type::op_mul, .value={ current, current + 1 } }};
        case '/': return std::optional<token> { token{ .type=token_type::op_div, .value={ current, current + 1 } }};
        case '^': return std::optional<token> { token{ .type=token_type::op_exp, .value={ current, current + 1 } }};
        case '-': return std::optional<token> { token{ .type=token_type::op_sub, .value={ current, current + 1 } }};
        case '+': return std::optional<token> { token{ .type=token_type::op_add, .value={ current, current + 1 } }};
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
        // std::println(stderr, "{}: consumed {}", sl.function_name(), tokens[index].value);
        return index++ < tokens.size();
    }

    constexpr std::optional<node> parse();
    constexpr std::optional<node> parse_expression();
    constexpr std::optional<node> parse_term();
    constexpr std::optional<node> parse_factor();
    constexpr std::optional<node> parse_var();

    constexpr std::optional<node> parse_constant(bool negate = false);
    constexpr std::optional<node> parse_symbol();

    constexpr static std::optional<node> parse(const std::span<const token> tokens);

private:
    parser() = default;
};

constexpr std::optional<node> parser::parse()
{
    return parse_expression();
}

// grammar:
//
// <expr> ::= <term> '+' <expr> | <term> '-' <expr> | <term>
// <term> ::= [-+]? <factor> '*' <term> | [-+]? <factor> '/' <term> | [-+]? <factor>
// <factor> ::= <var> ^ <factor> | <var>
// <var> ::= <constant> | <symbol> | '(' <expr> ')'
// <constant> ::= [0-9]+
// <symbol> ::= [A-Za-z]+

constexpr std::optional<node> parser::parse_expression()
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
    return std::optional<node> {
        op_node {
            std::make_unique<node>(std::move(term.value())),
            std::make_unique<node>(std::move(expr.value())),
            op_type
        }
    };
}

constexpr std::optional<node> parser::parse_term()
{
    const auto current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type == token_type::op_sub) {
        assert(consume());
        auto ret = parse_term();
        if (!ret.has_value())
            return {};

        return std::optional<node>{
            op_node {
                .left = std::make_unique<node>(constant_node{ 0 }),
                .right = std::make_unique<node>(std::move(ret.value())),
                .type = operation_type::sub,
            }
        };
    }

    if (current_token.type == token_type::op_add) {
        assert(consume());
        return parse_expression();
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
        return factor;

    const auto op_type = op_token.type == token_type::op_mul ? operation_type::mul : operation_type::div;
    return std::optional<node> {
        op_node{
            std::make_unique<node>(std::move(factor.value())),
            std::make_unique<node>(std::move(term.value())),
            op_type
        }
    };
}

constexpr std::optional<node> parser::parse_factor()
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

    return std::optional<node> {
        op_node{
            std::make_unique<node>(std::move(var.value())),
            std::make_unique<node>(std::move(factor.value())),
            operation_type::exp
        }
    };
}

constexpr std::optional<node> parser::parse_var()
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

constexpr std::optional<node> parser::parse_symbol()
{
    const auto& current_token_or = current();
    if (!current_token_or.has_value())
        return {};

    const auto& current_token = current_token_or.value();
    if (current_token.type != token_type::alpha)
        return {};

    assert(consume());

    return std::optional<node>{ symbol_node{ std::string(current_token.value) } };
}

constexpr std::optional<node> parser::parse_constant(bool negate)
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

    return std::optional<node>{ constant_node{ negate ? -val : val } };
}

constexpr std::optional<node> parser::parse(const std::span<const token> tokens)
{
    if (tokens.size() == 0)
        return {};

    parser p;
    p.tokens = tokens;
    return p.parse();
}

// ---------------------------------------- Interpreter

struct symbol_table
{
    struct node_t
    {

    };

    constexpr auto find(const std::string_view) const { return 0; }
};

using execution_result = int;

struct interpreter
{
    constexpr static execution_result interpret(const node& root_node,
                                                const symbol_table& symbol_table);
};

constexpr execution_result interpreter::interpret(const node& root_node,
                                                  const symbol_table& symbol_table)
{
    if(std::holds_alternative<op_node>(root_node)) {
        const auto& op = std::get<op_node>(root_node);
        const auto left_result = interpreter::interpret(*op.left, symbol_table);
        const auto right_result = interpreter::interpret(*op.right, symbol_table);
        switch(op.type) {
            case operation_type::mul: return execution_result{ left_result * right_result };
            case operation_type::div: return execution_result{ left_result / right_result };
            case operation_type::add: return execution_result{ left_result + right_result };
            case operation_type::sub: return execution_result{ left_result - right_result };
            case operation_type::exp: return execution_result{ (int)std::pow(left_result, right_result) };
        }
    }

    else if (std::holds_alternative<constant_node>(root_node)) {
        const auto& op = std::get<constant_node>(root_node);
        return execution_result{ op.value };
    }

    else if(std::holds_alternative<symbol_node>(root_node)) {
        const auto& op = std::get<symbol_node>(root_node);
        return execution_result{ symbol_table.find(op.value) };
    }

    std::unreachable();
}

}

using namespace mathc;

static void print_tree(const node& root_node)
{
    if(std::holds_alternative<op_node>(root_node)) {
        const auto& op = std::get<op_node>(root_node);
        std::print("o:(");

        if (op.left.get())
            print_tree(*op.left);

        std::print("{}", operation_type_to_string(op.type));

        if (op.right.get())
            print_tree(*op.right);

        std::print(")");
        return;
    }

    else if (std::holds_alternative<constant_node>(root_node)) {
        const auto& op = std::get<constant_node>(root_node);
        std::print("c:{}", op.value);
        return;
    }

    else if(std::holds_alternative<symbol_node>(root_node)) {
        const auto& op = std::get<symbol_node>(root_node);
        std::print("s:{}", op.value);
        return;
    }

    std::unreachable();
}

constexpr static execution_result test(const std::string_view source)
{
    auto tokens = lexer::lex(source).value();
    auto node = parser::parse(tokens).value();
    return interpreter::interpret(node, {});
}

static_assert(test("1+1") == 2);
static_assert(test("10+10") == 20);
static_assert(test("10+10-20") == 0);
static_assert(test("10+10-25") == -5);
static_assert(test("-25+10") == -15);
static_assert(test("-(25+10)") == -35);
static_assert(test("--25+10") == 35);

int main(int, const char* argv[])
{
    const auto tokens = lexer::lex(std::string_view{ argv[1] });
    if (!tokens.has_value())
        return 1;

    for(const auto& t : tokens.value())
        std::println("{} {}", token_type_str(t.type), t.value);

    const auto root_node_or = parser::parse(std::span{ tokens.value().begin(), tokens.value().end() });
    if (!root_node_or.has_value())
        return 1;

    const auto& root_node = root_node_or.value();
    print_tree(root_node);
    std::puts("");

    std::println("result: {}", interpreter::interpret(root_node, {}));
}
