#pragma once
#include <cmath>
void encode(std::vector<unsigned char> & base, uint32_t body, size_t field_size)
{
    for (size_t i = 0; i < field_size; i++) {
        base.push_back(static_cast<unsigned char>(body >> 8 * (field_size - 1)));
        body <<= 8;
    }
}
void encode(std::vector<unsigned char> & base, std::vector<unsigned char> body)
{
    for (size_t i = 0; i < body.size(); i++) {
        base.push_back(body[i]);
    }
}
void encode(std::vector<unsigned char> & base, unsigned char symbol)
{
    base.push_back(symbol);
}

void encode(std::vector<unsigned char> & base, TimeInForce time_in_force, int pos)
{
    base[pos] = base[pos] | 1;
    base.push_back(charAt(time_in_force));
}
void encode(std::vector<unsigned char> & base, Capacity capacity, int pos)
{
    base[pos] = base[pos] | 8;
    base.push_back(charAt(capacity));
}
void encode(std::vector<unsigned char> & base, const std::string & body, size_t field_size)
{
    size_t i = 0;
    while (i < body.size() && i < field_size) {
        base.push_back(body[i]);
        ++i;
    }
    while (i < field_size) {
        base.push_back(0x20);
        ++i;
    }
}
uint32_t convertPrice(OrdType price_type, double price)
{
    switch (price_type) {
    case OrdType::Market:
        return static_cast<uint32_t>(0x7FFFFFFF);
    case OrdType::Limit:
        return static_cast<uint32_t>(price * 10000 + copysign(1e-5, price));
    }
    assert(false);
    return 0;
}
uint32_t toString(const std::string & body)
{
    uint32_t result = 0;
    for (size_t i = 0; i < body.size(); i++) {
        result = result * 10 + (body[i] - '0');
    }
    return result;
}
