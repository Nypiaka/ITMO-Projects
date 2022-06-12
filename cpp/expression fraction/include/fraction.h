#pragma once

#include <cstdint>
#include <ostream>
struct Fraction
{
public:
    explicit operator double() const
    {
        return static_cast<double>(num) / static_cast<double>(denom);
    };

    Fraction(int64_t start_num)
        : num(start_num)
        , denom(1)
    {
        reduce();
    }
    template <typename T>
    Fraction(std::int64_t start_num, T start_denom)
        : num(start_denom > 0 ? start_num : -start_num)
        , denom(start_denom > 0 ? static_cast<uint64_t>(start_denom) : static_cast<uint64_t>(-start_denom))
    {
        reduce();
    }
    Fraction()
        : num(0)
        , denom(1)
    {
    }

    int64_t numerator() const;
    uint64_t denominator() const;
    std::string str() const;

    friend bool operator<(const Fraction & a, const Fraction & b)
    {
        return static_cast<double>(a) < static_cast<double>(b);
    }
    friend bool operator>(const Fraction & a, const Fraction & b)
    {
        return b < a;
    }
    friend bool operator>=(const Fraction & a, const Fraction & b)
    {
        return !(a < b);
    }
    friend bool operator<=(const Fraction & a, const Fraction & b)
    {
        return !(a > b);
    }
    friend bool operator==(const Fraction & a, const Fraction & b)
    {
        return a.numerator() * b.denominator() == a.denominator() * b.numerator();
    }
    friend bool operator!=(const Fraction & a, const Fraction & b)
    {
        return !(a == b);
    }

private:
    std::int64_t num;
    std::uint64_t denom;
    void reduce();
};
Fraction operator+(const Fraction & a, const Fraction & b);
Fraction operator*(const Fraction & a, const Fraction & b);
Fraction operator/(const Fraction & a, const Fraction & b);
Fraction operator-(const Fraction & a, const Fraction & b);
Fraction operator-(const Fraction & a);
std::ostream & operator<<(std::ostream & os, Fraction a);
void operator*=(Fraction & a, const Fraction & b);
void operator+=(Fraction & a, const Fraction & b);
void operator-=(Fraction & a, const Fraction & b);
void operator/=(Fraction & a, const Fraction & b);
