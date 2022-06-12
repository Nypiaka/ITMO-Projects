#include "genome.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stack>
#include <unordered_map>
#include <unordered_set>
namespace genome {
std::string assembly(const size_t k, const std::vector<std::string> & reads)
{
    if (k == 0 || reads.empty()) {
        return "";
    }
    if (reads.size() == 1) {
        return reads[0];
    }
    std::unordered_set<std::size_t> minus;
    std::vector<std::stack<std::size_t>> ways_table;
    std::vector<int> powers;
    std::vector<std::string_view> values;
    std::unordered_map<std::string_view, std::size_t> map;
    std::size_t marker = 0;
    std::size_t prev_min1 = -1;
    std::size_t res_min1 = -1;
    for (std::string const & current_string : reads) {
        const std::string_view current_string_view(current_string);
        for (std::size_t i = 0; i + k < current_string.size(); i++) {
            const std::string_view parent = current_string_view.substr(i, k);
            const std::string_view child = current_string_view.substr(i + 1, k);
            std::size_t prev_map_size = map.size();
            auto [parent_it, parent_result] = map.try_emplace(parent, marker);
            if (prev_map_size != map.size()) {
                marker++;
                ways_table.push_back(std::stack<std::size_t>());
                powers.push_back(0);
                prev_map_size = map.size();
                values.push_back(parent);
            }
            std::size_t parent_marker = parent_it->second;
            auto [child_it, child_result] = map.try_emplace(child, marker);
            if (prev_map_size != map.size()) {
                ways_table.push_back(std::stack<std::size_t>());
                powers.push_back(0);
                marker++;
                values.push_back(child);
            }
            std::size_t child_marker = child_it->second;
            if (res_min1 == parent_marker || res_min1 == child_marker) {
                res_min1 = prev_min1;
            }
            else if (prev_min1 == parent_marker || prev_min1 == child_marker) {
                prev_min1 = res_min1;
            }
            powers[parent_marker]++;
            powers[child_marker]--;
            if (powers[child_marker] == -1) {
                if (prev_min1 != static_cast<std::size_t>(-1)) {
                    prev_min1 = child_marker;
                }
                res_min1 = child_marker;
            }
            if (powers[parent_marker] == -1) {
                if (prev_min1 != static_cast<std::size_t>(-1)) {
                    prev_min1 = parent_marker;
                }
                res_min1 = parent_marker;
            }
            ways_table[child_marker].push(parent_marker);
        }
    }
    std::stack<std::size_t> stack;
    stack.push(powers[res_min1] == -1 ? res_min1 : prev_min1);
    std::string result;
    bool fl = true;
    while (!stack.empty()) {
        std::size_t iterator = stack.top();
        if (ways_table[iterator].empty()) {
            if (!fl) {
                result += values[iterator][values[iterator].size() - 1];
            }
            else {
                result = values[iterator];
                fl = false;
            }
            stack.pop();
        } //если top не имеет родителей (то есть не содержится в ways_table, либо по всем его родителям уже прошлись и его стэк пуст),
        //мы добавляем часть, которую даёт текущий узел в итоговый результат, иначе переходим в первого его родителя
        else {
            auto iterator_for_current_parent = ways_table[iterator].top();
            stack.push(iterator_for_current_parent);
            ways_table[iterator].pop();
        }
    }
    return result;
}
} // namespace genome