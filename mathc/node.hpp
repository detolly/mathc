#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include <number.hpp>

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

struct op_node;
struct constant_node;
struct symbol_node;
struct function_call_node;

using node = std::variant<op_node, constant_node, symbol_node, function_call_node>;

struct constant_node
{
    number value;
};

struct symbol_node
{
    std::string value;
};

struct function_call_node
{
    std::string function_name;
    std::vector<node> arguments;
};

struct op_node
{
    std::unique_ptr<node> left;
    std::unique_ptr<node> right;
    operation_type type;
};

// Utilities

template<typename T>
concept node_type = []<typename... Ts>(std::variant<Ts...>) {
    return (std::same_as<T, Ts> || ...);
}(node{});

template<node_type T, typename... Args>
constexpr static inline node make_node(Args&&... args)
{
    return node{ std::in_place_type_t<T>{}, std::forward<Args>(args)... };
}

template<node_type T, typename... Args>
constexpr static inline std::unique_ptr<node> make_unique_node(Args&&... args)
{
    return std::make_unique<node>(std::in_place_type_t<T>{}, std::forward<Args>(args)...);
}

constexpr static inline node copy_node(const auto& n);
constexpr static inline std::vector<node> copy_arguments(const function_call_node& op)
{
    std::vector<node> arguments;
    arguments.reserve(op.arguments.size());

    for(const auto& argument_node : op.arguments)
        arguments.emplace_back(copy_node(argument_node));

    return arguments;
}

constexpr static auto copy_function_node(const function_call_node& f)
{
    return make_node<function_call_node>(f.function_name,
                                         copy_arguments(f));
}

constexpr static struct
{
    constexpr static auto operator()(const op_node& op)
    {
        return make_node<op_node>(std::make_unique<node>(copy_node(*op.left)),
                                  std::make_unique<node>(copy_node(*op.right)),
                                  op.type);
    }
    constexpr static auto operator()(const function_call_node& op) { return copy_function_node(op); }
    constexpr static auto operator()(const symbol_node& op) { return make_node<symbol_node>(op); }
    constexpr static auto operator()(const constant_node& op) { return make_node<constant_node>(op); }
} copy_visitor{};

constexpr static inline node copy_node(const auto& n)
{
    return std::visit(copy_visitor, n);
}

constexpr static inline std::string_view operation_type_to_string_view(operation_type type)
{
    switch(type) {
        case operation_type::mul: return "*"sv;
        case operation_type::div: return "/"sv;
        case operation_type::add: return "+"sv;
        case operation_type::sub: return "-"sv;
        case operation_type::exp: return "^"sv;
    }

    std::unreachable();
}

using hash_t = std::size_t;
constexpr static std::uint32_t sleeve32 = 0x4D7192AE;
constexpr static std::uint64_t sleeve64 = 0xA2D109FE4AB123E9;

template<typename T> constexpr hash_t hash(const T&);

template<>
constexpr std::size_t hash<std::uint32_t>(const std::uint32_t& n) { return n ^ sleeve32; }
template<>
constexpr std::size_t hash<std::uint64_t>(const std::uint64_t& n) { return n ^ sleeve64; }
template<>
constexpr std::size_t hash<std::int64_t>(const std::int64_t& n) { return static_cast<std::uint64_t>( n); }
template<>
constexpr std::size_t hash<double>(const double& n) { return static_cast<std::uint64_t>(n); }
template<>
constexpr std::size_t hash<int>(const int& n) { return hash(static_cast<std::uint32_t>(n)); }
template<>
constexpr std::size_t hash<std::string_view>(const std::string_view& str)
{
    std::size_t hash = sleeve32;
    for (char c : str) {
        hash = ((hash << 5) ^ hash) ^ (static_cast<std::uint64_t>(c) * sleeve32);
    }
    return hash;
}
template<> constexpr std::size_t hash<node>(const node& n);

constexpr static struct
{
    constexpr static std::size_t operator()(const op_node& op)
    {
        const auto left_hash = hash<node>(*op.left);
        const auto right_hash = hash<node>(*op.right);

        const auto underlying = std::to_underlying(op.type);
        using underlying_t = std::remove_cv<decltype(underlying)>::type;
        const auto op_hash = hash<underlying_t>(underlying);

        const auto hash = left_hash ^ (right_hash ^ (op_hash << 4) + right_hash);
        return static_cast<std::size_t>(hash);
    }

    constexpr static std::size_t operator()(const constant_node& c)
    {
        return std::visit([&c](const auto& n) {
            return (hash(n) & 0xFFFFFFFE) | c.value.is_double();
        }, c.value.impl);
    }

    constexpr static std::size_t operator()(const function_call_node& f)
    {
        std::size_t digest = hash(std::string_view{ f.function_name });
        for(const auto& n : f.arguments) {
            const auto h = hash(n);
            digest = (digest ^ (h << 4)) + h;
        }
        return digest;
    }

    constexpr static std::size_t operator()(const symbol_node& sym)
    {
        return hash(std::string_view{ sym.value });
    }
} node_hasher;

template<>
constexpr std::size_t hash<node>(const node& n)
{
    return std::visit(node_hasher, n);
}

}

