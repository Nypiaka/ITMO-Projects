#include "fraction.h"

#include <numeric>
void Fraction::reduce()
{
    if (denom == 0) {
        num = num == 0 ? 0 : -num / std::abs(num);
    }
    else {
        uint64_t gcd = std::gcd(std::abs(num), denom);
        num /= static_cast<int64_t>(gcd);
        denom /= gcd;
    }
}
int64_t Fraction::numerator() const
{
    return num;
}
uint64_t Fraction::denominator() const
{
    return denom;
}

std::string Fraction::str() const
{
    return std::to_string(numerator()) + "/" + std::to_string(denominator());
}

Fraction operator+(const Fraction & a, const Fraction & b)
{
    if (a.numerator() != 0 && b.numerator() != 0) {
        int64_t anum = std::abs(a.numerator());
        int64_t bnum = std::abs(b.numerator());
        uint64_t first_num_second_denom_gcd = std::gcd(anum, b.denominator());
        uint64_t first_denom_second_num_gcd = std::gcd(a.denominator(), bnum);
        int64_t result_num_a = a.numerator() / static_cast<int64_t>(first_num_second_denom_gcd);
        int64_t result_num_b = b.numerator() / static_cast<int64_t>(first_denom_second_num_gcd);
        uint64_t result_denom_a = a.denominator() / first_denom_second_num_gcd;
        uint64_t result_denom_b = b.denominator() / first_num_second_denom_gcd;
        return Fraction(result_num_a * result_denom_b + result_num_b * result_denom_a, result_denom_b * result_denom_a);
    }
    else {
        if (a.numerator() != 0) {
            return a;
        }
        else {
            return b;
        }
    }
}
Fraction operator*(const Fraction & a, const Fraction & b)
{
    int64_t anum = std::abs(a.numerator());
    int64_t bnum = std::abs(b.numerator());
    uint64_t first_num_second_denom_gcd = std::gcd(anum, b.denominator());
    uint64_t first_denom_second_num_gcd = std::gcd(a.denominator(), bnum);
    return Fraction((a.numerator() / static_cast<int64_t>(first_num_second_denom_gcd)) *
                            (b.numerator() / static_cast<int64_t>(first_denom_second_num_gcd)),
                    (a.denominator() / first_denom_second_num_gcd) *
                            (b.denominator() / first_num_second_denom_gcd));
}
Fraction operator-(const Fraction & a, const Fraction & b)
{
    return a + (-b);
}
Fraction operator/(const Fraction & a, const Fraction & b)
{
    return a * Fraction(static_cast<std::int64_t>(b.denominator()), b.numerator());
}
Fraction operator-(const Fraction & a)
{
    return Fraction(-a.numerator(), a.denominator());
}

void operator*=(Fraction & a, const Fraction & b)
{
    a = a * b;
}
void operator-=(Fraction & a, const Fraction & b)
{
    a = a - b;
}
void operator+=(Fraction & a, const Fraction & b)
{
    a = a + b;
}
void operator/=(Fraction & a, const Fraction & b)
{
    a = a / b;
}
std::ostream & operator<<(std::ostream & os, Fraction a)
{
    os << a.numerator() << "/" << a.denominator();
    return os;
}
