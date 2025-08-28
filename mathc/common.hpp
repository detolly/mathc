#pragma once

#include <node.hpp>
#include <number.hpp>
#include <token.hpp>

namespace mathc
{

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-macros"

#define PROPAGATE_ERROR(x, expression)                      \
        auto&& x ## _result = (expression);                 \
        if (!x ## _result.has_value()) [[unlikely]]         \
            return x ## _result;                            \
        [[maybe_unused]] auto&& x = x ## _result .value()   \

#pragma GCC diagnostic pop

}
