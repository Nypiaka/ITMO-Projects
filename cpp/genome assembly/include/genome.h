#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace genome {
struct Node
{
    explicit Node(const std::string_view val)
        : value(val)
    {
    }
    const std::string_view value;
    int power = 0;
};
std::string assembly(size_t k, const std::vector<std::string> & reads);

} // namespace genome
