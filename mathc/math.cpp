#include <cassert>
#include <print>
#include <string_view>
#include <utility>
#include <variant>

#include <lexer.hpp>
#include <parser.hpp>
#include <interpreter.hpp>

using namespace mathc;

[[maybe_unused]] static void print_tree(const node& root_node)
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

#ifndef NO_TEST
consteval static auto evaluate(const std::string_view source)
{
    const auto vec = lexer::lex(source);
    assert(vec.size() > 0);

    const auto node_or = parser::parse(vec);
    assert(node_or.has_value());

    const auto& node = node_or.value();
    return interpreter::interpret(node, {});
}

consteval static bool test_equals(const std::string_view source, auto v) {
    const auto result = evaluate(source);
    if (result != v)
        assert(false && "asd");

    return true;
}

static_assert(test_equals("1+1", 2));
static_assert(test_equals("10+10", 20));
static_assert(test_equals("10+10-20", 0));
static_assert(test_equals("10+10-25", -5));
static_assert(test_equals("-25+10", -15));
static_assert(test_equals("-(25+10)", -35));
static_assert(test_equals("--25+10", 35));
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
static_assert(test_equals("2^(-2)", 1.0/4.0));
static_assert(test_equals("2^(-8)", 1.0/256.0));
#endif

int main(int argc, const char* argv[])
{
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wunsafe-buffer-usage"

    if (argc < 2) {
        std::println("Usage: {} {{expression}}", argv[0]);
        return 1;
    }
    const auto tokens = lexer::lex(std::string_view{ argv[1] });

    #pragma GCC diagnostic pop

    // for(const auto& t : tokens)
    //     std::println(stderr, "{} {}", token_type_str(t.type), t.value);

    const auto root_node_or = parser::parse(std::span{ tokens });
    if (!root_node_or.has_value())
        return 1;

    const auto& root_node = root_node_or.value();
    // print_tree(root_node);
    // std::puts("");

    std::println("{}", interpreter::interpret(root_node, {}));
}
