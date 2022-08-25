#include <bits/stdc++.h>
#include <iostream>
#include <unordered_map>
#include <unordered_set>
#include <vector>
std::vector<std::unordered_map<char, unsigned long long>> true_base;
std::vector<std::unordered_map<char, unsigned long long>> base;
std::unordered_map<std::size_t, unsigned long long> map_vershin;
std::vector<std::unordered_set<unsigned long long>> antibase;
std::unordered_set<unsigned long long> reached_from_start;
std::unordered_set<unsigned long long> reaches_terminal;
std::unordered_set<unsigned long long> dopusk;
std::vector<std::unordered_map<char, unsigned long long>> antirebra;
std::unordered_map<unsigned long long, std::size_t> colors;
std::unordered_map<unsigned long long, std::unordered_set<unsigned long long>> ways;
std::unordered_set<unsigned long long> w;
std::unordered_map<unsigned long long, unsigned long long> d;
void dfs(unsigned long long u) {
    reached_from_start.insert(u);
    for (auto iter : base[u]) {
        if (reached_from_start.count(iter.second) == 0) {
            dfs(iter.second);
        }
    }
}

unsigned long long dfs1(unsigned long long v) {
    if (w.count(v) != 0) {
        return d.find(v)->second;
    }
    unsigned long long sum = 0;
    w.insert(v);
    for (auto c : antirebra[v]) {
        sum = (sum % 1000000007 + dfs1(c.second) % 1000000007) % 1000000007;
    }
    d.insert({v, sum});
    return sum;
}
bool dfs2(unsigned long long v) {
    colors.find(v)->second = 1;
    for (auto u : true_base[v]) {
        if (colors.find(u.second)->second == 0) {
            if (dfs2(u.second)) {
                return true;
            }
        }
        if (colors.find(u.second)->second == 1) {
            return true;
        }
    }
    colors.find(v)->second = 2;
    return false;
}
void anti_dfs(unsigned long long u) {
    reaches_terminal.insert(u);
    for (auto iter : antibase[u]) {
        if (reaches_terminal.count(iter) == 0) {
            anti_dfs(iter);
        }
    }
}
int main() {
    std::ifstream ct("problem3.in");
    std::ofstream cot("problem3.out");
    std::unordered_set<char> alphabet;
    unsigned long long n, m, k;
    ct >> n >> m >> k;
    base.resize(n);
    antibase.resize(n);
    if (n == 0 || m == 0 || k == 0) {
        cot << 0;
        return 0;
    }
    for (std::size_t i = 0; i < k; i++) {
        std::size_t dop;
        ct >> dop;
        dopusk.insert(dop - 1);
    }
    for (std::size_t i = 0; i < m; i++) {
        std::size_t start;
        std::size_t end;
        char val;
        ct >> start >> end >> val;
        antibase[end - 1].insert(start - 1);
        base[start - 1].insert({val, end - 1});
    }
    dfs(0);
    for (auto iter : dopusk) {
        if (reached_from_start.count(iter) != 0) {
            anti_dfs(iter);
        }
    }

    std::unordered_map<unsigned long long, unsigned long long> sootv;
    unsigned long long mrk = 0;
    for (unsigned long long i = 0; i < n; i++) {
        if (reaches_terminal.count(i) != 0 && reached_from_start.count(i) != 0) {
            sootv.insert({i, mrk});
            mrk++;
        }
    }
    true_base.resize(mrk);
    for (unsigned long long i = 0; i < n; i++) {
        if (sootv.count(i) != 0) {
            for (auto symb : base[i]) {
                if (sootv.count(symb.second) != 0) {
                    true_base[sootv.find(i)->second].insert({symb.first, sootv.find(symb.second)->second});
                    alphabet.insert(symb.first);
                }
            }
        }
    }
    if (true_base.empty()) {
        cot << 0;
        return 0;
    }
    for (int i = 0; i < true_base.size(); i++) {
        colors.insert({i, 0});
    }
    for (int i = 0; i < true_base.size(); i++) {
        ways.insert({i, std::unordered_set<unsigned long long>()});
        for (auto it : true_base[i]) {
            ways.find(i)->second.insert(it.second);
        }
    }
    if (sootv.count(0) == 0) {
        cot << 0;
        return 0;
    };
    if (dfs2(sootv.find(0)->second)) {
        cot << -1;
        return 0;
    }
    std::unordered_set<unsigned long long> true_dopusk;

    for (auto iter : dopusk) {
        if (reached_from_start.count(iter) != 0) {
            true_dopusk.insert(sootv.find(iter)->second);
        }
    }

    unsigned long long result = 0;

    if (true_dopusk.empty()) {
        cot << 0;
        return 0;
    }
    d.insert({0, 1});
    w.insert(0);
    antirebra.resize(true_base.size());
    for (int i = 0; i < true_base.size(); i++) {
        for (auto it : true_base[i]) {
            antirebra[it.second].insert({it.first, i});
        }
    }
    for (auto it : true_dopusk) {
        result = (result % 1000000007 + dfs1(it) % 1000000007) % 1000000007;
    }
    cot << result;
    return 0;
}