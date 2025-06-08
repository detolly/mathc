#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include <node.hpp>

namespace mathc
{

struct vm
{
    constexpr void insert_symbol(const std::string_view symbol, const node& node)
    {
        for(auto& [name, n] : symbols) {
            if (symbol == name) {
                n = copy_node(node);
                return;
            }
        }

        symbols.emplace_back(symbol, copy_node(node));
    }

    constexpr std::optional<node> symbol_node(const std::string_view symbol) const
    {
        for(auto& [name, n] : symbols)
            if (name == symbol)
                return std::make_optional<node>(copy_node(n));

        return {};
    }

    std::vector<std::pair<std::string, node>> symbols{};
};

}
