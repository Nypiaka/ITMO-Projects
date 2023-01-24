#include <iostream>
#include <vector>
#include <fstream>
#include <algorithm>

class DSU {
public:
    bool has_one_parent(int first, int second, int &parent_f, int &parent_s) {
        int parent_first = first;
        int parent_second = second;
        while (kid_to_parent[parent_first] != parent_first) {
            parent_first = kid_to_parent[parent_first];
        }
        while (kid_to_parent[parent_second] != parent_second) {
            parent_second = kid_to_parent[parent_second];
        }
        parent_f = parent_first;
        parent_s = parent_second;
        return parent_second == parent_first;
    }

    void unite(int pf, int ps) {
        if (parent_to_size[pf] > parent_to_size[ps]) {
            parent_to_size[pf] += parent_to_size[ps];
            kid_to_parent[ps] = pf;
        } else {
            parent_to_size[ps] += parent_to_size[pf];
            kid_to_parent[pf] = ps;
        }
    }

    std::vector<int> kid_to_parent;
    std::vector<int> parent_to_size;
};

int main() {
    int n, m;
    long long s;
    std::ifstream fin("destroy.in");
    fin >> n >> m >> s;
    DSU dsu;
    for (int i = 0; i < n; i++) {
        dsu.kid_to_parent.emplace_back(i);
        dsu.parent_to_size.emplace_back(1);
    }
    std::vector<std::pair<long long, std::pair<int, std::pair<int, int>>>> G;
    for (int i = 0; i < m; i++) {
        int a, b;
        long long weight;
        fin >> a >> b >> weight;
        G.emplace_back(std::make_pair(weight, std::make_pair(i + 1, std::make_pair(a - 1, b - 1))));
    }
    fin.close();
    std::sort(G.begin(), G.end());
    std::reverse(G.begin(), G.end());
    bool in_tree[G.size()];
    for (auto &i: in_tree) {
        i = false;
    }
    int num_of_v = 0;
    for (int i = 0; i < G.size(); i++) {
        int pf;
        int ps;
        if (num_of_v != n - 1 && !dsu.has_one_parent(G[i].second.second.first, G[i].second.second.second, pf, ps)) {
            dsu.unite(pf, ps);
            in_tree[i] = true;
            num_of_v++;
        }
    }
    long long sum = 0;
    int iterator = G.size() - 1;
    long long final;
    std::vector<int> res;
    while (iterator >= 0) {
        if (!in_tree[iterator] && s - G[iterator].first >= sum) {
            sum += G[iterator].first;
            res.emplace_back(G[iterator].second.first);
            final = G[iterator].first;
        }
        iterator--;
    }
    if (sum > s) {
        sum -= final;
        res.pop_back();
    }
    std::sort(res.begin(), res.end());
    std::ofstream fout("destroy.out");
    fout << res.size() << std::endl;
    for (const auto &i: res) {
        fout << i << " ";
    }
    fout.close();
    return 0;
}