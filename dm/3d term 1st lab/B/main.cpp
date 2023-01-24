#include <string>
#include <iostream>
#include <deque>
#include <algorithm>

int main() {
    std::deque<std::size_t> a1;
    std::size_t a2;
    std::cin >> a2;
    bool a3[a2][a2];
    if (a2 > 0) {
        a3[0][0] = false;
    }

    for (std::size_t a4 = 1; a4 < a2; ++a4) {
        std::string a5;
        std::cin >> a5;
        std::size_t a6 = 0;
        for (auto e_at: a5) {
            a3[a4][a6] = (e_at == '1');
            a3[a6][a4] = (e_at == '1');
            a3[a4][a4] = false;
            a6++;
        }
    }
    for (std::size_t i = 0; i < a2; i++) {
        a1.push_back(i);
    }
    for (std::size_t a7 = 0; a7 < a2 * (a2 - 1); ++a7) {
        if (!a3[a1[0]][a1[1]]) {
            std::size_t a8 = 2;
            while ((a8 < a2 - 1) && (!a3[a1[0]][a1[a8]] || !a3[a1[1]][a1[a8 + 1]])) {
                a8++;
            }
            if (a8 == a2 - 1) {
                a8 = 2;
                while (!a3[a1[0]][a1[a8]]) { ++a8; }
            }
            std::reverse(a1.begin() + 1, a1.begin() + (int) a8 + 1);
        }
        a1.push_back(a1[0]);
        a1.pop_front();
    }
    for (auto a9: a1) {
        std::cout << a9 + 1 << " ";
    }
    return 0;
}