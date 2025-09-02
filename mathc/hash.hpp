#pragma once

#include <cstdint>
#include <string_view>

namespace mathc
{

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

}
