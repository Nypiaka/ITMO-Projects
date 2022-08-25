#include <fstream>
#include <iostream>
#include <unordered_map>

int main() {
    std::ifstream ct("problem1.in");
    std::ofstream cot("problem1.out");
    std::string str;
    ct >> str;
    std::size_t n, m, k;
    ct >> n >> m >> k;
    std::unordered_map<char, std::size_t> base[100000];
    std::unordered_map<std::size_t, bool> dopusk;
    for (std::size_t i = 0; i < k; ++i) {
        std::size_t dop;
        ct >> dop;
        dopusk.insert({dop - 1, true});
    }
    for (std::size_t i = 0; i < m; ++i) {
        std::size_t start;
        std::size_t end;
        char put;
        ct >> start >> end >> put;
        base[start - 1].insert({put, end - 1});
    }
    std::size_t iter_str = 0;
    std::size_t iter_map = 0;
    bool flag = true;
    while (true) {
        if (base[iter_map].count(str[iter_str]) == 0) {
            cot << "Rejects";
            return 0;
        } else {
            iter_map = base[iter_map].find(str[iter_str])->second;
            iter_str++;
            if (iter_str == str.size()) { break; }
        }
    }
    if (dopusk.count(iter_map) > 0) {
        cot << "Accepts";
    } else {
        cot << "Rejects";
    }
    return 0;
}