#pragma once

#include <string>
#include <string_view>
#include <vector>

namespace genome {
std::string assembly(size_t k, const std::vector<std::string> & reads);

} // namespace genome
