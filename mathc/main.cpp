#include <cassert>
#include <print>
#include <string_view>
#include <utility>
#include <variant>

#include <simplify.hpp>
#include <lexer.hpp>
#include <parser.hpp>
#include <token.hpp>

struct printer
{
    void operator()(const mathc::op_node& op)
    {
        std::print(stderr, "(");

        if (op.left.get())
            std::visit(printer{}, *op.left);

        std::print(stderr, "{}", operation_type_to_string(op.type));

        if (op.right.get())
            std::visit(printer{}, *op.right);

        std::print(stderr, ")");
    }

    void operator()(const mathc::constant_node& op)
    {
        std::print(stderr, "{}", op.value);
    }

    void operator()(const mathc::symbol_node& op)
    {
        std::print(stderr, "{}", op.value);
    }

    void operator()(const mathc::function_call_node& op)
    {
        std::print(stderr, "{}(", op.function_name);
        for(auto i = 0u; i < op.arguments.size(); i++) {
            const auto& argument = op.arguments[i];
            std::visit(printer{}, argument);
            if (i != op.arguments.size() - 1)
                std::print(stderr, ", ");
        }
        std::print(stderr, ")");
    }
};

static void print_tree(const mathc::node& root_node)
{
    std::visit(printer{}, root_node);
}

[[maybe_unused]]
consteval static auto evaluate(const std::string_view source)
{
    const auto vec = mathc::lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_result = mathc::parser::parse(vec);
    assert(node_result.has_value());

    const auto& node = node_result.value();
    auto vm = mathc::vm{};

    auto result = mathc::simplify(node, vm);
    assert(std::holds_alternative<mathc::constant_node>(result));

    return std::get<mathc::constant_node>(result).value;
}

#ifndef NO_TEST
consteval static bool test_equals(const std::string_view source, auto v)
{
    return evaluate(source).approx_equals(v);
}

static_assert(test_equals("1+1", 2));
static_assert(test_equals("10+10", 20));
static_assert(test_equals("10+10-20", 0));
static_assert(test_equals("10+10-25", -5));
static_assert(test_equals("-25+10", -15));
static_assert(test_equals("-(25+10)", -35));
static_assert(test_equals("-(-25+10)", 15));
static_assert(test_equals("-(25-10)", -15));
static_assert(test_equals("-2(2)", -4));
static_assert(test_equals("2(-2)", -4));
static_assert(test_equals("-2(-2)", 4));
static_assert(test_equals("1-1+1", 1));
static_assert(test_equals("1-2*1+1", 0));
static_assert(test_equals("1-(-2)*1+1", 4));
static_assert(test_equals("1-(1+1)", -1));
static_assert(test_equals("(1-1)+1", 1));
static_assert(test_equals("2.5*2", 5.0));
static_assert(test_equals("1.5^5", 7.59375));
static_assert(test_equals("1^5", 1));
static_assert(test_equals("2^2", 4));
static_assert(test_equals("2^3", 8));
static_assert(test_equals("sqrt(4)", 2));
static_assert(test_equals("sqrt(0)", 0));
static_assert(test_equals("log2(8)", 3));
static_assert(test_equals("2^(-2)", 1.0/4.0));
static_assert(test_equals("2^(-8)", 1.0/256.0));
static_assert(test_equals("(1/2)/2", 1.0 / 4.0));
static_assert(test_equals("(2)(2)", 4));
static_assert(test_equals("(2)2", 4));
static_assert(test_equals("2(2)", 4));
static_assert(test_equals("(2)*2", 4));
static_assert(test_equals("1/2/2", 1.0 / 4.0));
static_assert(test_equals("100/5/5", 4));
static_assert(test_equals("100/5/5", 4));
static_assert(test_equals("2 + 3", 5));
static_assert(test_equals("10 - 4", 6));
static_assert(test_equals("6 * 7", 42));
static_assert(test_equals("15 / 3", 5));
static_assert(test_equals("(2 + 3) * 4", 20));
static_assert(test_equals("10 / (2 + 3)", 2));
static_assert(test_equals("2 * 3 + 4", 10));
static_assert(test_equals("2 + 3 * 4", 14));
static_assert(test_equals("(2 * 3) + 4", 10));
static_assert(test_equals("2 * (3 + 4)", 14));
static_assert(test_equals("100 - 25 * 2", 50));
static_assert(test_equals("(100 - 25) * 2", 150));
static_assert(test_equals("8 / 2 + 3", 7));
static_assert(test_equals("8 + 2 / 2", 9));
static_assert(test_equals("(8 + 2) / 2", 5));
static_assert(test_equals("2 ^ 3", 8));
static_assert(test_equals("2 + 2 ^ 3", 10));
static_assert(test_equals("2 ^ 3 + 2", 10));
static_assert(test_equals("(2 ^ 3) * 2", 16));
static_assert(test_equals("2 ^ (3 * 2)", 64));
static_assert(test_equals("-5 + 3", -2));
static_assert(test_equals("5 + -3", 2));
static_assert(test_equals("-5 * -3", 15));
static_assert(test_equals("-10 / 2", -5));
static_assert(test_equals("(-10) / 2", -5));
static_assert(test_equals("10 / -2", -5));
static_assert(test_equals("2.5 * 4", 10));
static_assert(test_equals("10.0 / 4", 2.5));
static_assert(test_equals("3.14 + 2.86", 6));
static_assert(test_equals("3.14 * 2", 6.28));
static_assert(test_equals("10.5 - 0.5", 10));
static_assert(test_equals("(2.5 + 3.5) * 2", 12));
static_assert(test_equals("2.5 * (3 + 2)", 12.5));
static_assert(test_equals("100 / (2.5 * 2)", 20));
static_assert(test_equals("2 ^ 2 + 3 ^ 2", 13));
static_assert(test_equals("(2 + 3) ^ 2", 25));
static_assert(test_equals("2 ^ (2 + 1)", 8));
static_assert(test_equals("10 * (2 + 3 * (4 + 5))", 290));
static_assert(test_equals("(10 * (2 + 3)) * (4 + 5)", 450));
static_assert(test_equals("(2 + 3 * 4) ^ 2", 196));
static_assert(test_equals("2 + 3 * 4 ^ 2", 50));
static_assert(test_equals("(2 + 3) * 4 ^ 2", 80));
static_assert(test_equals("2 ^ 3 * 4 ^ 2", 128));
static_assert(test_equals("(2 ^ 3) * (4 ^ 2)", 128));
static_assert(test_equals("100 - 2 * (3 + 4 * (5 + 6))", 6));
static_assert(test_equals("(100 - 2) * (3 + 4 * 5)", 2254));
static_assert(test_equals("2.5 * (3.5 + 4.5 * (2 + 3))", 65));
static_assert(test_equals("(2.5 + 3.5) * (4.5 + 5.5)", 60));
static_assert(test_equals("(10 + 20) * (30 - 10) / (2 + 3)", 120));
static_assert(test_equals("2 ^ (3 + 4 * (5 - 2))", 32768));
#endif

int main(int argc, const char* argv[])
{
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wunsafe-buffer-usage"
    if (argc < 2) {
        std::println("Usage: {} {{expression}}", argv[0]);
        return 1;
    }
    const auto source = std::string_view{ argv[1] };
    #pragma GCC diagnostic pop

    const auto tokens = mathc::lexer::lex(source);
    // for(const auto& t : tokens)
    //     std::println(stderr, "{:20} {}", token_type_str(t.type), t.value);

    const auto root_node_result = mathc::parser::parse(std::span{ tokens });
    if (!root_node_result.has_value()) {
        const auto& error = root_node_result.error();
        std::println(stderr, "{} | token: {} {}", error.error, error.token.value, token_type_str(error.token.type));
        return 1;
    }

    const auto& root_node = root_node_result.value();
    print_tree(root_node);
    std::puts("");

    auto vm = mathc::vm{};
    const auto result = mathc::simplify(root_node, vm);
    if (!std::holds_alternative<mathc::constant_node>(result)) {
        print_tree(result);
        std::puts("");
        return 0;
    }

    std::println("{}", std::get<mathc::constant_node>(result).value);
    return 0;
}
