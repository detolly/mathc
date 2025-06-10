#pragma once

#include <cstdint>
#include <optional>
#include <variant>

#include <lexer.hpp>

namespace mathc
{

struct number
{
    constexpr explicit number(std::int64_t i) : impl(i) {}
    constexpr explicit number(double d) : impl(d) {}

    constexpr static inline number from_int(std::int64_t num) { return number{ num }; }
    constexpr static inline number from_double(double num) { return number{ num }; }
    constexpr static inline std::optional<number> from_token(const token& t);

    constexpr inline bool is_int() const { return std::holds_alternative<std::int64_t>(impl); }
    constexpr inline bool is_double() const { return std::holds_alternative<double>(impl); }

    constexpr inline auto as_int() const { return std::get<std::int64_t>(impl); }
    constexpr inline auto as_double() const { return std::get<double>(impl); }

    constexpr inline auto promote_to_double() const
    {
        return std::visit([](const auto num){ return static_cast<double>(num); }, impl);
    }

    constexpr number operator*(const number& other) const;
    constexpr number operator+(const number& other) const;
    constexpr number operator-(const number& other) const;
    constexpr number operator/(const number& other) const;
    constexpr number operator^(const number& other) const;

    constexpr number& operator=(double other);
    constexpr number& operator=(std::int64_t other);

    constexpr bool approx_equals(const auto other, const double acceptable_difference = 0.000001) const;

    constexpr bool operator==(double other) const;
    constexpr bool operator==(std::int64_t other) const;
    constexpr bool operator==(int other) const;

    std::variant<double, std::int64_t> impl{ 0 };
};

}

#ifndef NO_NUMBER_IMPL

#include <cmath>
#include <format>

#include <math.hpp>

namespace mathc
{

constexpr static inline std::optional<number> parse_double(const std::string_view str)
{
    double result = 0.0;
    double sign = 1.0;
    double fraction = 0.1;
    bool has_seen_decimal = false;

    for (const auto c : str) {
        if (c == '.') {
            if (has_seen_decimal) return {};
            has_seen_decimal = true;
            continue;
        }

        if (!has_seen_decimal)
            result = result * 10.0 + (c - '0');
        else {
            result += (c - '0') * fraction;
            fraction *= 0.1;
        }
    }

    return std::make_optional<number>(sign * result);
}

constexpr inline std::optional<number> number::from_token(const token& t)
{
    const auto value = std::string_view{ t.value };
    if (t.has_decimal) {
        if consteval {
            return parse_double(t.value);
        }

        double val;
        const auto res = std::from_chars(value.begin(), value.end(), val);
        if (res.ec != std::errc{})
            return {};

        return std::make_optional<number>(val);
    }

    std::int64_t val;
    const auto res = std::from_chars(value.begin(), value.cend(), val);
    if (res.ec != std::errc{})
        return {};

    return std::make_optional<number>(val);
}

constexpr inline bool number::operator==(double other) const
{
    if (!is_double())
        return false;

    const auto diff = std::abs(as_double() - other);
    return diff < std::numeric_limits<double>::epsilon();
}

constexpr bool number::approx_equals(const auto other, const double acceptable_difference) const
{
    return std::visit([other, acceptable_difference](const auto d){
        return (std::abs(static_cast<double>(d) - static_cast<double>(other))) < acceptable_difference;
    }, impl);
}

constexpr inline bool number::operator==(std::int64_t other) const { return is_int() && as_int() == other; }
constexpr inline bool number::operator==(int other) const { return operator==(static_cast<std::int64_t>(other)); }

template<bool promote_to_double = false, typename Callable>
constexpr static inline number visit_two(Callable&& c, const number& a, const number& b)
{
    const auto op = [&c](const auto a_val, const auto b_val) {
        if constexpr(!promote_to_double &&
                     std::is_same_v<std::int64_t, std::remove_cvref_t<decltype(a_val)>> &&
                     std::is_same_v<std::int64_t, std::remove_cvref_t<decltype(b_val)>>)
            return number{ c(static_cast<std::int64_t>(a_val),
                             static_cast<std::int64_t>(b_val)) };
        else
            return number{ c(static_cast<double>(a_val),
                             static_cast<double>(b_val)) };
    };

    return std::visit([&](const auto& a_val, const auto& b_val) {
        return op(a_val, b_val);
    }, a.impl, b.impl);
}

constexpr inline number number::operator*(const number& other) const
{
    return visit_two([](const auto a, const auto b){ return a * b; }, *this, other);
}

constexpr inline number number::operator+(const number& other) const
{
    return visit_two([](const auto a, const auto b){ return a + b; }, *this, other);
}

constexpr inline number number::operator-(const number& other) const
{
    return visit_two([](const auto a, const auto b){ return a - b; }, *this, other);
}

constexpr inline number number::operator/(const number& other) const
{
    return visit_two<true>([](const auto a, const auto b) { return a / b; }, *this, other);
}

constexpr inline number number::operator^(const number& exponent) const
{
    return visit_two([](const auto a, const auto b){ return math::pow(a, b); }, *this, exponent);
}

constexpr inline number& number::operator=(const double other) { impl = other; return *this; }
constexpr inline number& number::operator=(const std::int64_t other) { impl = other; return *this; }

}

template<>
struct std::formatter<mathc::number, char>
{
    template<class C>
    constexpr C::iterator parse(C& ctx) const { return ctx.begin(); }

    template<class F>
    F::iterator format(const mathc::number& num, F& c) const
    {
        return std::visit([&c](auto n){ return std::ranges::copy(std::format("{}", n), c.out()).out; }, num.impl);
    }
};

#endif


