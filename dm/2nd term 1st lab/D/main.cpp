#include <fstream>
#include <iostream>
#include <stack>
#include <unordered_map>
#include <vector>

const unsigned long long MOD = 1000000000 + 7;
std::vector<std::vector<unsigned long long>> pow(const std::vector<std::vector<unsigned long long>> &a, const std::vector<std::vector<unsigned long long>> &b) {
    std::vector<std::vector<unsigned long long>> base2(a.size(), std::vector<unsigned long long>(a.size()));
    for (unsigned long long row = 0; row < a.size(); row++) {
        for (unsigned long long col = 0; col < b.size(); col++) {
            unsigned long long result = 0;
            for (unsigned long long inner = 0; inner < a.size(); inner++) {
                result = (result + (a[row][inner] % MOD * b[inner][col] % MOD) % MOD) % MOD;
            }
            base2[row][col] = result;
        }
    }
    return base2;
}
int main() {
    std::ifstream ct("problem4.in");
    std::ofstream cot("problem4.out");
    unsigned long long n, m, k, l;
    ct >> n >> m >> k >> l;
    std::vector<std::vector<unsigned long long>> base(n, std::vector<unsigned long long>(n));
    std::vector<std::vector<unsigned long long>> result(n, std::vector<unsigned long long>(n));
    for (unsigned long long i = 0; i < n; i++) {
        for (unsigned long long j = 0; j < n; j++) {
            base[i][j] = 0;
            if (i != j) {
                result[i][j] = 0;
            } else {
                result[i][j] = 1;
            }
        }
    }
    std::vector<unsigned long long> dopusk;
    for (std::size_t i = 0; i < k; ++i) {
        unsigned long long dop;
        ct >> dop;
        dopusk.push_back(dop - 1);
    }
    for (unsigned long long i = 0; i < m; ++i) {
        unsigned long long start;
        unsigned long long end;
        char put;
        ct >> start >> end >> put;
        base[start - 1][end - 1] = (base[start - 1][end - 1] + 1) % MOD;
    }
    unsigned long long deg = l;
    while (deg) {
        if (deg % 2 == 0) {
            deg /= 2;
            base = pow(base, base);
        } else {
            deg--;
            result = pow(result, base);
        }
    }


    unsigned long long kolvo = 0;
    for (unsigned long long i : dopusk) {
        kolvo = (kolvo + result[0][i]) % MOD;
    }
    cot << kolvo;
    return 0;
}