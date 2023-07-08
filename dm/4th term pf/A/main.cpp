#include <iostream>
#include <unordered_set>
using namespace std;
int main() {
    int n;
    int m;
    cin >> n >> m;
    n++;
    m++;
    long long firstPF[n];
    long long secondPF[m];
    for (int i = 0; i < n; i++) {
        cin >> firstPF[i];
    }
    for (int i = 0; i < m; i++) {
        cin >> secondPF[i];
    }
    cout << max(n, m) - 1 << "\n";
    for (int i = 0; i < max(m, n); i++) {
        long long res = 0;
        if (i < n) { res = (firstPF[i] % 998244353 + res % 998244353) % 998244353; }
        if (i < m) { res = (secondPF[i] % 998244353 + res % 998244353) % 998244353; }
        cout << res << " ";
    }
    cout << "\n";
    long long mult[m + n - 1];
    for (int i = 0; i < m + n - 1; i++) {
        mult[i] = 0;
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            mult[i + j] = ((mult[i + j]) % 998244353 + ((firstPF[i]) % 998244353) * ((secondPF[j]) % 998244353) % 998244353) % 998244353;
        }
    }
    cout << m + n - 2 << "\n";
    for (int i = 0; i < m + n - 1; i++) {
        cout << mult[i] << " ";
    }
    cout << "\n";
    long long qoutient[1000];
    for (long long &i: qoutient) {
        i = 0;
    }
    qoutient[0] = firstPF[0];
    for (int i = 1; i < 1000; i++) {
        long long part = 0;
        for (int k = 0; k <= i - 1; k++) {
            part = (part % 998244353 + (qoutient[k]) % 998244353 * ((i - k >= m) ? 0 : ((secondPF[i - k]) % 998244353)) % 998244353) % 998244353;
        }
        qoutient[i] = ((i >= n ? 0 : (firstPF[i])) % 998244353 - (part) % 998244353 + 998244353) % 998244353;
    }
    for (long long i: qoutient) {
        cout << i << " ";
    }
    return 0;
}
