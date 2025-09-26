#pragma once

#include <string_view>
#include <algorithm>

namespace mathc
{

template<auto s, auto _size = s - 1>
struct fixed_string
{
    consteval fixed_string() = default;
    consteval fixed_string(const char(&string)[s])
    {
        std::copy_n(string, s, str);
    }

    template<auto other>
    consteval auto concat()
    {
        fixed_string<s + other.size()> ret;
        std::copy_n(data(), size(), ret.str);
        std::copy_n(other.data(), other.size(), ret.str + size());
        return ret;
    }

    consteval static auto size() { return _size; }
    consteval std::string_view view() const { return { begin(), end() }; }

    consteval auto data() const { return str;  }
    consteval auto begin() const { return str;  }
    consteval auto end() const { return str + _size; }

    consteval auto operator[](const auto i) const { return str[i]; }

    char str[s]{ 0 };
};

template<auto s>
fixed_string(const char (&)[s]) -> fixed_string<s>;

}
