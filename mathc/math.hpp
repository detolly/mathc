#pragma once

#include <cstdint>
#include <cmath>
#include <limits>
#include <type_traits>

#define NO_NUMBER_IMPL
#include <number.hpp>
#undef NO_NUMBER_IMPL

namespace math
{

using namespace mathc;

namespace detail
{

constexpr static inline bool double_equal(const double a, const double b)
{
    constexpr static auto epsilon = std::numeric_limits<double>::epsilon();
    return (a < b + epsilon && a > b - epsilon);
}

constexpr static auto NUM_ITER = 20;

constexpr static inline double sqrt_newton(const double x, const double curr, const double prev, int iter = NUM_ITER)
{
    return (iter == 0 || double_equal(curr, prev)) ?
        curr :
        sqrt_newton(x, 0.5 * (curr + x / curr), curr, iter - 1);
}

constexpr static inline double exp(const double x, int iter = NUM_ITER)
{
    double result = 1.0, term = 1.0;
    for (int i = 1; i <= iter; ++i) {
        term *= x / i;
        result += term;
    }
    return result;
}

constexpr static inline double ln_newton(const double x, const double curr, const double prev, int iter = NUM_ITER)
{
    return (iter == 0 || double_equal(curr, prev)) ?
        curr :
        ln_newton(x, curr - (exp(curr) - x) / exp(curr), curr, iter - 1);
}

constexpr static inline double pow(const double base, const double exponent)
{
    if (double_equal(0.0, base) && exponent <= 0.0)
        return 0.0;

    if (double_equal(exponent, 1.0))
        return base;

    if (exponent < 0.0)
        return 1.0  / pow(base, -exponent);

    auto int_exp = static_cast<std::int64_t>(exponent);
    double result = 1.0;

    const auto frac_exp = exponent - static_cast<double>(int_exp);

    auto base_sq = base;
    while (int_exp > 0) {
        if (int_exp & 1)
            result *= base_sq;
        base_sq *= base_sq;
        int_exp >>= 1;
    }

    if (frac_exp != 0.0)
        result *= exp(frac_exp * ln_newton(base, base, 0.0));

    return result;
}

}

constexpr static inline number sqrt(const double num)
{
    if (!std::is_constant_evaluated())
        return number{ std::sqrt(num) };

    if (num < 0)
        assert(false);

    if (detail::double_equal(num, 0.0))
        return number::from_int(0);

    return number{ detail::sqrt_newton(num, num, 0.0) };
}

constexpr static inline number sqrt(const std::int64_t num)
{
    return sqrt(static_cast<double>(num));
}

constexpr static inline number pow(const double base, const double exp)
{
    if (!std::is_constant_evaluated())
        return number{ std::pow(base, exp) };

    return number{ detail::pow(base, exp) };
}

constexpr static inline number pow(const std::int64_t base, const std::int64_t exp)
{
    if (exp < 0)
        return number{ 1.0 / static_cast<double>(pow(base, -exp).as_int()) };

    auto ret = base;
    for(auto i = 1; i < exp; i++)
        ret *= base;

    return number{ ret };
}

constexpr static inline number log(double num)
{
    if (!std::is_constant_evaluated())
        return number{ std::log(num) };

    return number{ detail::ln_newton(num, num, 0.0) };
}

constexpr static inline number log2(double num)
{
    if (!std::is_constant_evaluated())
        return number{ std::log2(num) };

    return number{ detail::ln_newton(num, num, 0.0) / detail::ln_newton(2.0, 2.0, 0.0) };
}

}
