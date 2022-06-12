#include "requests.h"

#include "encode.h"

#include <cmath>
#include <string>
std::vector<unsigned char> create_enter_order_request(
        const std::string & cl_or_id /*cl_ord_id*/,
        const Side side /*side*/,
        const double quantity /*volume*/,
        const double price /*price*/,
        const std::string & order_book /*symbol*/,
        const OrdType price_type /*ord_type*/,
        const TimeInForce time_in_force /*time_in_force*/,
        const Capacity capacity /*capacity*/,
        const std::string & firm /*firm*/,
        const std::string & user /*user*/
)
{
    /* Write your code here */;
    std::vector<unsigned char> result;
    encode(result, static_cast<unsigned char>('O'));
    encode(result, cl_or_id, 14);
    encode(result, charAt(side));
    encode(result, static_cast<uint32_t>(quantity), 4);
    encode(result, toString(order_book), 4);
    encode(result, convertPrice(price_type, price), 4);
    encode(result, firm, 4);
    encode(result, user, 6);
    encode(result, {0, 0, 0, 0});
    encode(result, time_in_force, 38);
    encode(result, capacity, 38);
    return result;
}

std::vector<unsigned char> create_replace_order_request(
        const std::string & old_cl_ord_id /*old_cl_ord_id*/,
        const std::string & newCl /*new_cl_ord_id*/,
        const double total_volume /*total_volume*/,
        const double price /*price*/,
        const TimeInForce time_in_force /*time_in_force*/,
        const std::string & user /*user*/
)
{
    /* Write your code here */;
    std::vector<unsigned char> result;
    encode(result, static_cast<unsigned char>('U'));
    encode(result, old_cl_ord_id, 14);
    encode(result, newCl, 14);
    encode(result, static_cast<uint32_t>(round(total_volume)), 4);
    encode(result, static_cast<uint32_t>(price * 10000 + copysign(1e-5, price)), 4);
    encode(result, user, 6);
    encode(result, {0, 0, 0, 0});
    encode(result, time_in_force, 43);
    return result;
}
