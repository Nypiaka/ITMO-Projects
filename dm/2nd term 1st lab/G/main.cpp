#include <fstream>
#include <iostream>
#include <queue>
#include <unordered_map>
std::unordered_map<char, std::size_t> base1[2000];
std::unordered_map<std::size_t, bool> dopusk1;
std::unordered_map<char, std::size_t> base2[2000];
std::unordered_map<std::size_t, bool> dopusk2;
std::unordered_map<std::size_t, bool> nums1;
std::unordered_map<std::size_t, bool> nums2;
bool visited[2000][2000];
bool bfsEquivalenceCheck() {
    std::queue<std::pair<std::size_t, std::size_t>> q;
    q.push({0, 0});
    while (!q.empty()) {
        auto first = q.front();
        q.pop();
        std::size_t u = first.first;
        std::size_t v = first.second;
        if (dopusk1.count(u) != dopusk2.count(v)) {
            return false;
        }
        visited[u][v] = true;
        for (auto c : "abcdefghijklmnopqrstuvwxyz") {
            if (!visited[base1[u][c]][base2[v][c]]) {
                q.push({base1[u][c], base2[v][c]});
            }
        }
    }
    return true;
}

int main() {
    std::ifstream ct("equivalence.in");
    std::ofstream cot("equivalence.out");
    std::size_t n, m, k;
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
        nums1.insert({start - 1, true});
        nums1.insert({end - 1, true});
        base1[start - 1].insert({put, end - 1});
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
        nums2.insert({start - 1, true});
        nums2.insert({end - 1, true});
        base2[start - 1].insert({put, end - 1});
    }
    for (auto c : "abcdefghijklmnopqrstuvwxyz") {
        base1[1999].insert({c, 1999});
        base2[1999].insert({c, 1999});
        for (auto i : nums1) {
            if (base1[i.first].count(c) == 0) {
                base1[i.first].insert({c, 1999});
            }
        }
        for (auto j : nums2) {
            if (base2[j.first].count(c) == 0) {
                base2[j.first].insert({c, 1999});
            }
        }
    }
    bool res = bfsEquivalenceCheck();
    if (res) {
        cot << "YES";
        return 0;
    }
    cot << "NO";
    return 0;
}