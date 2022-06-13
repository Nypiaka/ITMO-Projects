#include "genome.h"

#include <algorithm>
#include <iostream>
#include <sstream>
#include <stack>
#include <unordered_map>
namespace genome {
std::string assembly(const size_t k, const std::vector<std::string> & reads)
{
    if (k == 0 || reads.empty()) {
        return "";
    }
    if (reads.size() == 1) {
        return reads[0];
    }
    std::unordered_map<std::string_view, std::stack<std::string_view>> ways_table;
    std::unordered_map<std::string_view, Node> map;
    for (std::string const & current_string : reads) {
        const std::string_view current_string_view(current_string);
        for (std::size_t i = 0; i + k < current_string.size(); i++) {
            const std::string_view parent = current_string_view.substr(i, k);
            const std::string_view child = current_string_view.substr(i + 1, k);
            auto [parent_it, parent_result] = map.try_emplace(parent, parent);
            auto [child_it, child_result] = map.try_emplace(child, child);
            parent_it->second.power++;
            child_it->second.power--;
            std::stack<std::string_view> current_stack;
            auto [it, inserted] = ways_table.try_emplace(child, current_stack);
            it->second.push(parent);
        }
    }
    std::stack<Node> stack;
    for (auto const & [key, val] : map) {
        if (val.power == -1) {
            stack.push(val);
            break;
        }
    }
    std::string result;
    bool fl = true;
    std::string tail;
    while (!stack.empty()) {
        Node iterator = stack.top();
        auto it = ways_table.find(iterator.value);
        if (it == ways_table.end() || it->second.empty()) {
            if (!fl) {
                result += iterator.value[iterator.value.size() - 1];
            }
            else {
                result = iterator.value;
                fl = false;
            }
            stack.pop();
        } //если top не имеет родителей (то есть не содержится в ways_table, либо по всем его родителям уже прошлись и его стэк пуст),
        //мы добавляем часть, которую даёт текущий узел в итоговый результат, иначе переходим в первого его родителя
        else {
            auto iterator_for_current_parent = map.find(it->second.top());
            if (iterator_for_current_parent != map.end()) {
                stack.push(iterator_for_current_parent->second);
            }
            it->second.pop();
            iterator.power--;
        }
    }
    return result;
}
} // namespace genome
