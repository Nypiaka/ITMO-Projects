#include <iostream>
#include <vector>
using namespace std;
long long modd = 104857601;
vector<long long> mult_poly(const vector<long long> &first_poly,
                            const vector<long long> &second_poly) {
    vector<long long> mult(first_poly.size() + second_poly.size() - 1, 0);
    for (long long i = 0; i < first_poly.size(); i++) {
        for (long long j = (i % 2 == 0 ? 0 : 1); j < second_poly.size(); j += 2) {
            mult[i + j] += (((first_poly[i] + modd) % modd) * ((second_poly[j] + modd) % modd) + modd) % modd;
        }
    }
    return mult;
}

long long algos(long long n, long long k, vector<long long> &a, vector<long long> &Q) {
    while (n >= k) {
        for (long long i = k; i <= 2 * k - 1; i++) {
            long long sum = 0;
            for (int j = 1; j <= k; j++) {
                sum = (sum + (((-Q[j] + modd) % modd) * ((a[i - j] + modd) % modd) + modd) % modd + modd) % modd;
            }
            a[i] = (sum + modd) % modd;
        }

        vector<long long> minus_Q(Q.size());
        for (long long i = 0; i < minus_Q.size(); i++) {
            if (i % 2 != 0) minus_Q[i] = (-Q[i] + modd) % modd;
            else
                minus_Q[i] = Q[i];
        }

        auto R = mult_poly(Q, minus_Q);

        for (long long i = (n % 2 == 0) ? 0 : 1; i < a.size(); i += 2) {
            a[i / 2] = a[i];
        }
        for (long long i = 0; i < Q.size(); i++) {
            Q[i] = R[i * 2];
        }
        n /= 2;
    }
    return a[n] % modd;
}

int main() {
    long long k, n;
    cin >> k >> n;
    vector<long long> a(2 * k + 10, 0);
    vector<long long> c(k);
    for (int i = 0; i < k; i++) {
        cin >> a[i];
    }
    for (int i = 0; i < k; i++) {
        cin >> c[i];
    }
    vector<long long> q(k + 1);
    q[0] = 1;
    for (int i = 0; i < k; i++) {
        q[i + 1] = -c[i];
    }
    cout << algos(n - 1, k, a, q);
}
