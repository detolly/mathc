#pragma once

#include <string_view>
#include <algorithm>

namespace mathc
{

template<auto s, auto _size = s - 1>
struct fixed_string
{
    consteval fixed_string(const char(&string)[s])
    {
        std::copy_n(string, s, str);
    }

    consteval static auto size() { return _size; }
    consteval std::string_view view() const { return { begin(), end() }; }

    consteval auto begin() const { return str;  }
    consteval auto end() const { return str + _size; }

    consteval auto operator[](const auto i) const { return str[i]; }

    char str[s]{ 0 };
};

template<auto s>
fixed_string(const char (&)[s]) -> fixed_string<s, s - 1>;

}
