#include <algorithm>
#include <iostream>
#include <numeric>
#include <vector>


int main() {
    int n;
    std::cin >> n;
    std::vector<int> result;
    result.emplace_back(1);
    int iter = 2;
    while (result.size() != n) {
        int l = -1;
        int r = result.size();
        while (true) {
            int half = (r + l) / 2;
            std::cout.flush();
            std::cout << "1 " << iter << ' ' << result[half] << std::endl;
            std::string ans;
            std::cin >> ans;
            if (ans == "YES") {
                r = half;
            } else { l = half; }
            if (r - l <= 1) { break; }
        }
        result.insert(result.begin() + r, iter);
        iter++;
    }
    std::cout << "0 ";
    for (auto i: result) {
        std::cout << i << " ";
    }
    return 0;
}