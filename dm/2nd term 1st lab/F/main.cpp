#include <fstream>
#include <iostream>
#include <unordered_map>
std::unordered_map<char, std::size_t> base1[200000];
std::unordered_map<std::size_t, bool> dopusk1;
bool anti_devil1[200000];
std::unordered_map<char, std::size_t> base2[200000];
std::unordered_map<std::size_t, bool> dopusk2;
bool visited[200000];
bool anti_devil2[200000];
std::size_t sootvetstvie[200000];
bool dfs(std::size_t first, std::size_t second) {
    visited[first] = true;
    if ((dopusk1.find(first) != dopusk1.end() && dopusk2.find(second) == dopusk2.end()) ||
        (dopusk1.find(first) == dopusk1.end() && dopusk2.find(second) != dopusk2.end())) {
        return false;
    }
    sootvetstvie[first] = second;
    bool result = true;
    for (auto cq : base1[first]) {
        std::size_t t1 = cq.second;
        std::size_t t2;
        if (base2[second].find(cq.first) != base2[second].end()) {
            t2 = base2[second].find(cq.first)->second;
        } else {
            return false;
        }
        if (anti_devil1[t1] != anti_devil2[t2]) { return false; }
        if (visited[t1]) {
            result = (result & (t2 == sootvetstvie[t1]));
        } else {
            result = result & dfs(t1, t2);
        }
    }
    return result;
}

int main() {
    std::ifstream ct("isomorphism.in");
    std::ofstream cot("isomorphism.out");
    std::size_t n, m, k;
    for (std::size_t i = 0; i < 200000; ++i) {
        anti_devil2[i] = false;
        anti_devil1[i] = false;
    }
    ct >> n >> m >> k;
    for (std::size_t i = 0; i < k; ++i) {
        std::size_t dop;
        ct >> dop;
        dopusk1.insert({dop - 1, true});
    }
    for (std::size_t i = 0; i < m; ++i) {
        std::size_t start;
        std::size_t end;
        char put;
        ct >> start >> end >> put;
        base1[start - 1].insert({put, end - 1});
        if (start != end) {
            anti_devil1[start - 1] = true;
        }
    }
    ct >> n >> m >> k;
    for (std::size_t i = 0; i < k; ++i) {
        std::size_t dop;
        ct >> dop;
        dopusk2.insert({dop - 1, true});
    }
    for (std::size_t i = 0; i < m; ++i) {
        std::size_t start;
        std::size_t end;
        char put;
        ct >> start >> end >> put;
        base2[start - 1].insert({put, end - 1});
        if (start != end) {
            anti_devil2[start - 1] = true;
        }
    }
    bool res = dfs(0, 0);
    if (res) {
        cot << "YES";
        return 0;
    }
    cot << "NO";
    return 0;
}