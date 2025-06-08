#pragma once

#include <optional>
#include <string_view>
#include <vector>

#include <node.hpp>

namespace mathc
{

struct vm
{
    constexpr std::optional<void> insert_symbol(const std::string_view symbol, node&& node);
    constexpr std::optional<node> symbol_node(const std::string_view) const
    {
        return {};
        // return std::make_optional<node>(std::in_place_type_t<constant_node>{}, number{ 0.0 });
    }

    std::vector<std::pair<std::string, node>> symbols{};
};

}
