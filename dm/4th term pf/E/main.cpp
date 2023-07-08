#include <iostream>
#include <vector>
using namespace std;


vector<long long> mult_poly(const vector<long long> &first_poly,
                            const vector<long long> &second_poly) {
    vector<long long> mult(first_poly.size() + second_poly.size() - 1, 0);
    for (int i = 0; i < first_poly.size(); i++) {
        for (int j = 0; j < second_poly.size(); j++) {
            mult[i + j] += first_poly[i] * second_poly[j];
        }
    }
    return mult;
}

vector<long long> pow_poly(const vector<long long> &poly, int pow) {
    if (pow == 0)
        return vector<long long>(1, 1);
    if (pow % 2 == 1)
        return mult_poly(pow_poly(poly, pow - 1), poly);
    auto tmp = pow_poly(poly, pow / 2);
    return mult_poly(tmp, tmp);
}

int main() {
    int r, d;
    cin >> r >> d;
    long long coeffs[d + 1];
    for (int i = 0; i < d + 1; i++) {
        cin >> coeffs[i];
    }
    auto Q = pow_poly({1, -r}, d + 1);
    long long powers[Q.size()];
    powers[0] = 1;
    for (int i = 1; i < Q.size(); i++) {
        powers[i] = powers[i - 1] * r;
    }
    vector<long long> A(Q.size(), 0);
    for (int i = 0; i < Q.size(); i++) {
        long long prev_i = 1;
        for (int j = 0; j < d + 1; j++) {
            A[i] += prev_i * coeffs[j] * powers[i];
            prev_i *= i;
        }
    }
    auto P = mult_poly(Q, A);
    cout << d << "\n";
    for (int i = 0; i < d + 1; i++) {
        cout << P[i] << " ";
    }
    cout << "\n";
    cout << Q.size() - 1 << "\n";
    for (const auto &p: Q) {
        cout << p << " ";
    }
}
