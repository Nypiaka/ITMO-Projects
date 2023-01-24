#include <iostream>
#include <deque>
#include <algorithm>
#include <unordered_map>
#include <unordered_set>
#include <queue>

int main() {
    std::unordered_set<int> was_in_way;
    std::unordered_map<int, std::unordered_set<int>> G;
    int n;
    int m;
    std::cin >> n >> m;
    std::queue<int> queue;
    int way[n];
    int colors[n];
    int max_deg = 0;
    int min_deg = n - 1;
    int min_index = 0;
    bool stop = true;
    for (int i = 0; i < m; i++) {
        int start;
        int end;
        std::cin >> start >> end;
        G[start - 1].insert(end - 1);
        G[end - 1].insert(start - 1);
        max_deg = std::max(max_deg, static_cast<int>(std::max(G[start - 1].size(), G[end - 1].size())));
    }
    for (const auto &g: G) {
        if (min_deg > static_cast<int>(g.second.size())) {
            min_deg = static_cast<int>(g.second.size());
            min_index = g.first;
        }
        stop = stop && (g.second.size() == n - 1);
    }
    if (stop) {
        std::cout << (max_deg % 2 == 1 ? max_deg : max_deg + 1) << std::endl;
        for (int i = 0; i < n; i++) {
            std::cout << i + 1 << std::endl;
        }
        return 0;
    }
    queue.push(min_index);
    was_in_way.insert(min_index);
    std::size_t way_iterator = 0;
    while (!queue.empty()) {
        way[way_iterator] = queue.front();
        for (auto cur_node: G[queue.front()]) {
            if (!was_in_way.count(cur_node)) {
                was_in_way.insert(cur_node);
                queue.push(cur_node);
            }
        }
        queue.pop();
        way_iterator++;
    }
    colors[way[n - 1]] = 0;
    for (int i = 0; i < n - 1; i++) {
        colors[way[i]] = -1;
    }
    for (int i = n - 2; i > -1; i--) {
        int cur_colors[max_deg + 1];
        for (int il = 0; il < max_deg + 1; il++) {
            cur_colors[il] = 0;
        }
        for (auto cur: G[way[i]]) {
            if (colors[cur] != -1) {
                cur_colors[colors[cur]] = 1;
            }
        }
        int result_color;
        for (int j = 0; j < max_deg + 1; j++) {
            if (cur_colors[j] == 0) {
                result_color = j;
                break;
            }
        }
        colors[way[i]] = result_color;
    }
    std::cout << (max_deg + 1 - max_deg % 2) << std::endl;
    for (auto i: colors) {
        std::cout << i + 1 << std::endl;
    }
    return 0;
}