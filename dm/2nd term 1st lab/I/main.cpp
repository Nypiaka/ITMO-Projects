#include <bits/stdc++.h>
#include <fstream>
#include <iostream>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <vector>
std::vector<std::unordered_map<char, std::size_t>> base;
std::vector<std::unordered_set<std::size_t>> antibase;
std::unordered_set<std::size_t> reached_from_start;
std::unordered_set<std::size_t> reaches_terminal;
std::unordered_set<std::size_t> dopusk;
void dfs(std::size_t u) {
    reached_from_start.insert(u);
    for (auto iter : base[u]) {
        if (reached_from_start.count(iter.second) == 0) {
            dfs(iter.second);
        }
    }
}
void anti_dfs(std::size_t u) {
    reaches_terminal.insert(u);
    for (auto iter : antibase[u]) {
        if (reaches_terminal.count(iter) == 0) {
            anti_dfs(iter);
        }
    }
}
struct hash_pair {
    template<class T1, class T2>
    size_t operator()(const std::pair<T1, T2> &p) const {
        auto hash1 = std::hash<T1>{}(p.first);
        auto hash2 = std::hash<T2>{}(p.second);

        if (hash1 != hash2) {
            return hash1 ^ hash2;
        }
        return hash1;
    }
};
int main() {
    std::ifstream ct("fastminimization.in");
    std::ofstream cot("fastminimization.out");
    std::unordered_map<std::pair<char, std::size_t>, std::unordered_set<std::size_t>, hash_pair> Inv;
    std::unordered_set<char> alphabet;
    std::vector<std::unordered_set<std::size_t>> P;
    std::size_t n, m, k;
    ct >> n >> m >> k;
    base.resize(n);
    antibase.resize(n);
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
    std::vector<std::unordered_map<char, std::size_t>> true_base;
    std::unordered_map<std::size_t, std::size_t> sootv;
    std::size_t mrk = 0;
    for (std::size_t i = 0; i < n; i++) {
        if (reaches_terminal.count(i) != 0 && reached_from_start.count(i) != 0) {
            sootv.insert({i, mrk});
            mrk++;
        }
    }
    true_base.resize(mrk);
    for (std::size_t i = 0; i < n; i++) {
        if (sootv.count(i) != 0) {
            for (auto symb : base[i]) {
                if (sootv.count(symb.second) != 0) {
                    true_base[sootv.find(i)->second].insert({symb.first, sootv.find(symb.second)->second});
                    alphabet.insert(symb.first);
                }
            }
        }
    }

    for (std::size_t i = 0; i < true_base.size(); i++) {
        for (auto iter : true_base[i]) {
            if (Inv.count({iter.first, true_base[i].find(iter.first)->second}) == 0) {
                Inv.insert({{iter.first, true_base[i].find(iter.first)->second}, std::unordered_set<std::size_t>()});
            }
            Inv.find({iter.first, true_base[i].find(iter.first)->second})->second.insert(i);
        }
    }
    std::unordered_set<std::size_t> true_dopusk;
    for (auto iter : dopusk) {
        if (reached_from_start.count(iter) != 0) {
            true_dopusk.insert(sootv.find(iter)->second);
        }
    }
    std::size_t Class[sootv.size()];
    if (!true_dopusk.empty()) {
        P.emplace_back(true_dopusk);
    }
    std::unordered_set<std::size_t> antidopusk;
    for (std::size_t i = 0; i < sootv.size(); i++) {
        if (true_dopusk.count(i) == 0) {
            antidopusk.insert(i);
            Class[i] = 1;
        } else {
            Class[i] = 0;
        }
    }
    if (!antidopusk.empty()) {
        P.emplace_back(antidopusk);
    }
    std::queue<std::pair<char, std::unordered_set<std::size_t>>> queue;
    for (auto c : alphabet) {
        queue.push({c, P[0]});
        if (P.size() > 1) {
            queue.push({c, P[1]});
        }
    }
    while (!queue.empty()) {
        std::unordered_set<std::size_t> C = queue.front().second;
        char a = queue.front().first;
        queue.pop();
        std::unordered_map<std::size_t, std::unordered_set<std::size_t>> Involved;
        for (auto q : C) {
            if (Inv.count({a, q}) != 0) {
                for (auto r : Inv.find({a, q})->second) {
                    std::size_t i = Class[r];
                    if (Involved.count(i) == 0) {
                        Involved.insert({i, std::unordered_set<std::size_t>()});
                    }
                    Involved.find(i)->second.insert(r);
                }
            }
        }
        for (const auto &i1 : Involved) {
            if (Involved.find(i1.first)->second.size() < P[i1.first].size()) {
                P.emplace_back();
                std::size_t j = P.size() - 1;
                for (auto r1 : Involved.find(i1.first)->second) {
                    P[i1.first].erase(r1);
                    P[j].insert(r1);
                }
                if (P[j].size() > P[i1.first].size()) {
                    std::unordered_set<std::size_t> cur = P[i1.first];
                    P[i1.first] = P[j];
                    P[j] = cur;
                }
                for (auto r2 : P[j]) {
                    Class[r2] = j;
                }
                for (auto c : alphabet) {
                    queue.push({c, P[j]});
                }
            }
        }
    }
    for (std::size_t i = 0; i < P.size(); i++) {
        if (P[i].count(0) != 0) {
            std::unordered_set<std::size_t> swaper = P[i];
            P[i] = P[0];
            P[0] = swaper;
            for (auto zero_it : P[0]) {
                Class[zero_it] = 0;
            }
            for (auto swap : P[i]) {
                Class[swap] = i;
            }
            break;
        }
    }
    std::vector<std::unordered_map<char, std::size_t>> result_base;
    result_base.resize(P.size());
    std::size_t result_iter = 0;
    std::unordered_set<std::size_t> result_dopusk;
    for (auto iter : P) {
        for (auto cur_char : true_base[*std::begin(P[result_iter])]) {
            result_base[result_iter].insert({cur_char.first, Class[true_base[*std::begin(P[result_iter])].find(cur_char.first)->second]});
        }
        result_iter++;
    }
    for (auto iter : true_dopusk) {
        result_dopusk.insert(Class[iter]);
    }
    std::size_t rebra = 0;
    for (const auto &c : result_base) {
        rebra += c.size();
    }
    cot << result_base.size() << " " << rebra << " " << result_dopusk.size() << std::endl;
    for (auto dop : result_dopusk) {
        cot << dop + 1 << " ";
    }
    cot << std::endl;
    for (std::size_t i = 0; i < result_base.size(); i++) {
        for (auto a : result_base[i]) {
            cot << (i + 1) << " " << (result_base[i].find(a.first)->second + 1) << " " << a.first << std::endl;
        }
    }
    return 0;
}