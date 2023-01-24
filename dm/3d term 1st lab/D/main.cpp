#include <string>
#include <iostream>
#include <deque>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>

int main() {

    std::size_t a2;
    std::cin >> a2;
    if (a2 == 1) {
        std::cout << 1;
        return 0;
    }
    std::unordered_map<std::size_t, std::unordered_set<std::size_t>> node_to_kids;
    std::unordered_map<std::size_t, std::unordered_set<std::size_t>> node_to_parents;
    std::unordered_map<std::size_t, std::size_t> node_to_max_num_of_current_way;
    node_to_kids.insert({0, std::unordered_set<std::size_t>()});
    node_to_parents.insert({0, std::unordered_set<std::size_t>()});
    for (std::size_t iter = 0; iter < a2; iter++) {
        node_to_kids.insert({iter, std::unordered_set<std::size_t>()});
        node_to_parents.insert({iter, std::unordered_set<std::size_t>()});
        node_to_max_num_of_current_way.insert({iter, 0});
    }

    for (std::size_t a4 = 1; a4 < a2; ++a4) {
        std::string a5;
        std::cin >> a5;
        std::size_t a6 = 0;
        for (auto e_at: a5) {
            if (e_at == '1') {
                node_to_kids[a4].insert(a6);
                node_to_parents[a6].insert(a4);
            } else {
                node_to_kids[a6].insert(a4);
                node_to_parents[a4].insert(a6);
            }
            ++a6;
        }
    }
    std::size_t way[a2];
    std::size_t nums_in_way = 1;
    way[0] = 0;
    for (std::size_t i = 1; i < a2; i++) {
        if (node_to_kids[0].count(i)) {
            node_to_max_num_of_current_way[i]++;
        }
    }
    std::unordered_set<std::size_t> already_used_in_way;
    already_used_in_way.insert(0);
    while (nums_in_way != a2) {
        std::size_t max_num_of_cur_way = 0;
        std::size_t result_num;
        for (auto iter: node_to_max_num_of_current_way) {
            if (!already_used_in_way.count(iter.first) && iter.second >= max_num_of_cur_way) {
                max_num_of_cur_way = iter.second;
                result_num = iter.first;
            }
        }
        already_used_in_way.insert(result_num);
        for (int j = nums_in_way - 1; j >= 0 && j >= max_num_of_cur_way; j--) {
            way[j + 1] = way[j];
        }
        way[max_num_of_cur_way] = result_num;
        for (auto iter: node_to_max_num_of_current_way) {
            if (!already_used_in_way.count(iter.first)) {
                if (!node_to_parents[iter.first].count(result_num)) {
                    node_to_max_num_of_current_way[iter.first] = std::min(max_num_of_cur_way,
                                                                          node_to_max_num_of_current_way[iter.first]);
                } else {
                    node_to_max_num_of_current_way[iter.first] = std::max(max_num_of_cur_way + 1,
                                                                          node_to_max_num_of_current_way[iter.first]);
                }
            }
        }
        nums_in_way++;
    }

//    for (auto i: way) {
//        std::cout << i << " ";
//    }

    std::size_t cycle[a2];
    std::size_t part_way[a2];
    int part_way_iterator = 0;
    int cur_cycle_length = 0;
    cycle[0] = way[0];
    int max_duga = -1;
    for (int i = 1; i < a2; i++) {
        if (node_to_kids[way[i]].count(way[0])) {
            max_duga = i;
        }
    }
    for (int i = 0; i <= max_duga; i++) {
        cycle[i] = way[i];
    }
    for (int i = max_duga + 1; i < a2; i++) {
        part_way[i - max_duga - 1] = way[i];
    }
    int part_way_start = 0;
    cur_cycle_length = max_duga + 1;
    while (cur_cycle_length != a2) {
        bool cont = true;
        for (int i = 0; (i < cur_cycle_length) && cont; i++) {
            if (node_to_kids[part_way[part_way_start]].count(cycle[i])) {
                for (int j = cur_cycle_length - 1; j >= i; j--) {
                    cycle[j + 1] = cycle[j];
                }
                cycle[i] = part_way[part_way_start];
                cur_cycle_length++;
                part_way_start++;
                cont = false;
            }
        }
        bool contin = true;
        for (int i1 = part_way_start + 1; cont && (i1 < part_way_iterator) && contin; i1++) {
            for (int i = 0; contin && (i < cur_cycle_length); i++) {
                if (node_to_kids[part_way[i1]].count(cycle[i])) {
                    for (int j = cur_cycle_length - 1; j >= i; j--) {
                        cycle[j + i1 - part_way_start + 1] = cycle[j];
                    }
                    int part_way_start1 = part_way_start;
                    for (int i2 = 0; i2 < i1 - part_way_start1 + 1; i2++) {
                        cycle[i + i2] = part_way[part_way_start1 + i2];
                        cur_cycle_length++;
                        part_way_start++;
                    }
                    contin = false;
                }
            }
        }
    }
    std::cout << std::endl;
    for (auto i: cycle) {
        std::cout << i + 1 << " ";
    }
    return 0;
}