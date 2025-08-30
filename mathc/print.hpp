
#include <print>
#include <variant>

#include <node.hpp>

namespace mathc
{

struct printer
{
    void operator()(const mathc::op_node& op)
    {
        std::print(stderr, "(");

        if (op.left.get())
            std::visit(printer{}, *op.left);

        std::print(stderr, "{}", operation_type_to_string_view(op.type));

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

inline static void print_tree(const mathc::node& root_node)
{
    std::visit(printer{}, root_node);
}

}
