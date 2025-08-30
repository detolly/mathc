
#include <node.hpp>

namespace mathc
{

template<auto n>
struct pattern_impl
{
    constexpr static auto get_node() { return n; }
};

struct pattern
{
    
};

struct strategy
{
    void rewrite(const node&);
};

template<typename left, typename right, auto rewriter>
struct op_strategy : public strategy
{
    void rewrite(const node&);
};

#define OP_STRATEGY(_left, _right, _func)                                           \
    op_strategy<_left,                                                              \
                _right,                                                             \
                []([[maybe_unused]] const _left& l,                                 \
                   [[maybe_unused]] const _right& r,                                \
                   [[maybe_unused]] operation_type t) {                             \
                    _func                                                           \
                }>{}

constexpr static auto strategies = std::initializer_list<strategy>
{
    OP_STRATEGY(constant_node, constant_node, return node{ constant_node{ number::from_int(0) } }; ),
};

}
