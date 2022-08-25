#include <fstream>
#include <iostream>
#include <stack>
#include <unordered_map>
#include <vector>
int main() {
    std::ifstream ct("problem2.in");
    std::ofstream cot("problem2.out");
    std::string str;
    ct >> str;
    std::size_t n, m, k;
    ct >> n >> m >> k;
    std::vector<std::unordered_map<char, std::vector<std::size_t>>> base;
    base.resize(10000);
    std::vector<std::size_t> dopusk;
    std::unordered_map<std::size_t, std::unordered_map<std::size_t, bool>> where_can_go;
    for (std::size_t i = 0; i < k; ++i) {
        std::size_t dop;
        ct >> dop;
        dopusk.push_back(dop - 1);
    }
    for (std::size_t i = 0; i < m; ++i) {
        std::size_t start;
        std::size_t end;
        char put;
        ct >> start >> end >> put;
        if (base[start - 1].count(put) == 0) {
            base[start - 1].insert({put, std::vector<std::size_t>{end - 1}});
        } else {
            base[start - 1].find(put)->second.push_back(end - 1);
        }
    }
    where_can_go.insert({-1, std::unordered_map<std::size_t, bool>()});
    where_can_go.find(-1)->second.insert({0, true});
    for (std::size_t i = 0; i < str.length(); ++i) {
        where_can_go.insert({i, std::unordered_map<std::size_t, bool>()});
        for (auto iter : where_can_go.find(i - 1)->second) {
            if (base[iter.first].find(str[i]) != base[iter.first].end()) {
                for (auto iter_in_current_map : base[iter.first].find(str[i])->second) {
                    where_can_go.find(i)->second.insert({iter_in_current_map, true});
                }
            }
        }
    }
    for (std::size_t &i : dopusk) {
        if (where_can_go.find(str.size() - 1)->second.count(i) != 0) {
            cot << "Accepts";
            return 0;
        }
    }
    cot << "Rejects";
    return 0;
}