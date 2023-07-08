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
    int k;
    cin >> k;
    vector<long long> A(k);
    vector<long long> C(k);
    for (int i = 0; i < k; i++) {
        cin >> A[i];
    }
    for (int i = 0; i < k; i++) {
        cin >> C[i];
    }
    vector<long long> Q(k + 1);
    Q[0] = 1;
    for (int i = 1; i < k + 1; i++) {
        Q[i] = -C[i - 1];
    }
    auto P = mult_poly(Q, A);
    int first_not_zero = 0;
    for (int i = k - 1; i >= 0; i--) {
        if (P[i] != 0) {
            first_not_zero = i;
            break;
        }
    }
    cout << first_not_zero << "\n";
    for (int i = 0; i < first_not_zero + 1; i++) {
        cout << P[i] << " ";
    }
    cout << "\n";
    cout << Q.size() - 1 << "\n";
    for (const auto &elem: Q) {
        cout << elem << " ";
    }
}
