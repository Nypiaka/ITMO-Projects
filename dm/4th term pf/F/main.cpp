#include <iostream>
#include <unordered_map>
#include <vector>
using namespace std;

int main() {
    vector<int> weights;
    long long divider = 1000000007;
    int k, m;
    cin >> k >> m;
    long long nums_to_weight[m + 1];
    long long mapp[m + 1];
    for (int i = 0; i < m + 1; i++) {
        mapp[i] = -1;
    }
    for (int i = 0; i < k; ++i) {
        int weight;
        cin >> weight;
        weights.emplace_back(weight);
    }
    nums_to_weight[0] = 1;
    mapp[0] = 1;
    long long results[m];
    for (int j = 1; j <= m; ++j) {
        long long num_of_trees_for_current_weight = 0;
        for (int current_weight: weights) {
            if (current_weight == j) {
                num_of_trees_for_current_weight =
                        (num_of_trees_for_current_weight + 1) % divider;
            } else {
                int delta = j - current_weight;
                if (delta >= 0) {
                    if (mapp[delta] == -1) {
                        mapp[delta] = 0;
                        for (int i = 0; delta >= 0 && i <= delta / 2; ++i) {
                            long long left = (nums_to_weight[i]);
                            long long right = (nums_to_weight[delta - i]);
                            num_of_trees_for_current_weight = (num_of_trees_for_current_weight +
                                                               ((((left) *
                                                                  (right)) %
                                                                 divider) *
                                                                ((delta % 2 == 0 &&
                                                                  i == delta / 2)
                                                                         ? 1
                                                                         : 2)) %
                                                                       divider) %
                                                              divider;
                            mapp[delta] += (((((left) *
                                               (right)) %
                                              divider) *
                                             ((delta % 2 == 0 &&
                                               i == delta / 2)
                                                      ? 1
                                                      : 2)) %
                                            divider) %
                                           divider;
                        }
                    } else {
                        num_of_trees_for_current_weight = (num_of_trees_for_current_weight + mapp[delta]) % divider;
                    }
                }
            }
        }
        nums_to_weight[j] = num_of_trees_for_current_weight % divider;
        results[j - 1] = num_of_trees_for_current_weight % divider;
    }
    for (const auto &num: results) {
        printf("%lld ", num);
    }
    return 0;
}