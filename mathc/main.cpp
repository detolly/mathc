#include <cassert>
#include <print>
#include <string_view>
#include <utility>
#include <variant>

#include <interpreter.hpp>
#include <lexer.hpp>
#include <parser.hpp>
#include <token.hpp>

using namespace mathc;

static void print_tree(const node& root_node)
{
    if(std::holds_alternative<op_node>(root_node)) {
        const auto& op = std::get<op_node>(root_node);
        std::print(stderr, "(");

        if (op.left.get())
            print_tree(*op.left);

        std::print(stderr, "{}", operation_type_to_string(op.type));

        if (op.right.get())
            print_tree(*op.right);

        std::print(stderr, ")");
        return;
    }

    else if (std::holds_alternative<constant_node>(root_node)) {
        const auto& op = std::get<constant_node>(root_node);
        std::print(stderr, "{}", op.value);
        return;
    }

    else if(std::holds_alternative<symbol_node>(root_node)) {
        const auto& op = std::get<symbol_node>(root_node);
        std::print(stderr, "{}", op.value);
        return;
    }

    else if(std::holds_alternative<function_call_node>(root_node)) {
        const auto& op = std::get<function_call_node>(root_node);
        std::print(stderr, "{}(", op.function_name);
        for(auto i = 0u; i < op.arguments.size(); i++) {
            const auto& argument = op.arguments[i];
            print_tree(argument);
            if (i != op.arguments.size() - 1)
                std::print(stderr, ", ");
        }
        std::print(stderr, ")");
        return;
    }

    std::unreachable();
}

[[maybe_unused]]
consteval static auto evaluate(const std::string_view source)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_result = parser::parse(vec);
    assert(node_result.has_value());

    const auto& node = node_result.value();
    auto vm = mathc::vm{};

    return interpreter::simplify(node, vm);
}

#ifndef NO_TEST
consteval static bool test_equals(const std::string_view source, auto v) {
    return std::get<number>(evaluate(source).value()).approx_equals(v);
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
static_assert(test_equals("sqrt(4)", 2.0));
static_assert(test_equals("sqrt(0)", 0));
static_assert(test_equals("log2(8)", 3.0));
static_assert(test_equals("2^(-2)", 1.0/4.0));
static_assert(test_equals("2^(-8)", 1.0/256.0));
static_assert(test_equals("(1/2)/2", 1.0 / 4.0));
static_assert(test_equals("(2)(2)", 4));
static_assert(test_equals("(2)2", 4));
static_assert(test_equals("(2)*2", 4));
static_assert(test_equals("1/2/2", 1.0 / 4.0));
static_assert(test_equals("100/5/5", 4.0));
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

    const auto tokens = lexer::lex(source);
    // for(const auto& t : tokens)
    //     std::println(stderr, "{:20} {}", token_type_str(t.type), t.value);

    const auto root_node_result = parser::parse(std::span{ tokens });
    if (!root_node_result.has_value()) {
        const auto& error = root_node_result.error();
        std::println(stderr, "{} | token: {} {}", error.error, error.token.value, token_type_str(error.token.type));
        return 1;
    }

    const auto& root_node = root_node_result.value();
    print_tree(root_node);
    std::puts("");

    auto vm = mathc::vm{};
    const auto result_or_error = interpreter::simplify(root_node, vm);
    if (!result_or_error.has_value()) {
        std::println(stderr, "{}", result_or_error.error().error);
        return 1;
    }

    const auto& result = result_or_error.value();
    if (std::holds_alternative<node>(result)) {
        print_tree(std::get<node>(result));
        std::puts("");
        return 0;
    }

    std::println("{}", std::get<number>(result));
    return 0;
}
