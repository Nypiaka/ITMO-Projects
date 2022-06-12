#include <algorithm>
#include <bits/stdc++.h>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

struct sort_struct
{
    std::vector<std::string> values;
    std::string current_string;
};

bool comp(sort_struct & a, sort_struct & b)
{
    return std::lexicographical_compare(a.values.begin(), a.values.end(), b.values.begin(), b.values.end());
}

void print(std::vector<sort_struct> & sorting_strings)
{
    if (!sorting_strings.empty()) {
        for (const sort_struct & sort_part : sorting_strings)
            std::cout << sort_part.current_string << std::endl;
    }
}

void sort_with_k(std::istream & f, std::size_t start_k, std::size_t end_k, std::string separator)
{
    std::vector<sort_struct> sorting_strings;
    for (auto [iterator, current] = std::tuple<std::size_t, std::string>{{},
                                                                         {}};
         std::getline(f, current);
         ++iterator) {
        sorting_strings.emplace_back();
        sorting_strings[iterator].current_string = current;
        std::stringstream this_string(current);
        std::string intermediate;
        std::size_t iter = 0;
        if (!separator.empty()) {
            while (getline(this_string, intermediate, separator[0])) {
                if (!intermediate.empty()) {
                    if (iter >= start_k - 1 && iter <= end_k - 1) {
                        sorting_strings[iterator].values.push_back(intermediate);
                    }
                    iter++;
                }
            }
        }
        else {
            while (!this_string.eof()) {
                this_string >> intermediate;
                if (iter >= start_k - 1 && iter <= end_k - 1) {
                    sorting_strings[iterator].values.push_back(intermediate);
                }
                iter++;
            }
        }
    }
    std::sort(sorting_strings.begin(), sorting_strings.end(), comp);
    print(sorting_strings);
}

void sort_k_t(std::istream & f, std::size_t start_k, std::size_t end_k, const std::string & separator)
{
    if (start_k == static_cast<std::size_t>(-1)) {
        std::vector<std::string> strings;
        std::string current;
        while (std::getline(f, current)) {
            strings.push_back(current);
        }
        std::sort(strings.begin(), strings.end());
        for (const std::string & s : strings) {
            std::cout << s << '\n';
        }
    }
    else {
        sort_with_k(f, start_k, end_k, separator);
    }
}

int main(int argc, char ** argv)
{
    std::vector<std::string_view> args;
    std::vector<std::string_view> parsed_args;
    for (int i = 1; i < argc; i++) {
        args.emplace_back(argv[i]);
    }
    std::size_t start_k = static_cast<std::size_t>(-1);
    std::size_t end_k = static_cast<std::size_t>(-1);
    std::string separator;
    std::string name;
    for (std::size_t j = 0; j < args.size(); ++j) {
        if ((args[j].substr(0, 2) == "-k") ||
            (args[j].substr(0, 5) == "--key")) {
            if ((args[j].substr(0, 2) == "-k" && args[j].size() != 2) ||
                (args[j].substr(0, 5) == "--key" && args[j].size() != 5)) {
                std::size_t k_pos = 0;
                if (args[j].substr(0, 2) == "-k")
                    k_pos = 2;
                else
                    k_pos = 5;
                if (args[j].substr(k_pos).find(',') == static_cast<std::size_t>(-1)) {
                    start_k = std::stoull(std::string(args[j].substr(k_pos)));
                    end_k = start_k;
                }
                else {
                    start_k = std::stoull(std::string(args[j].substr(0, args[j].substr(k_pos).find(',') - 1)));
                    end_k = std::stoull(std::string(args[j].substr(args[j].substr(k_pos).find(',') + 1)));
                }
            }
            else {
                start_k = std::stoull(std::string(args[j + 1]));
                end_k = start_k;
                j++;
            }
        }
        else if (args[j].substr(0, 15) == "--field-separator" || args[j].substr(0, 2) == "-t") {
            if (args[j].substr(0, 2) == "-t" && args[j].size() != 2) {
                separator = std::string(args[j].substr(2));
            }
            else if (args[j].substr(0, 15) == "--field-separator" && args[j].size() != 15) {
                separator = std::string(args[j].substr(16));
            }
            else {
                separator = &args[j + 1][0];
                j++;
            }
        }
        else {
            name = args[j];
        }
    }
    if (name.empty() || name == "-") {
        sort_k_t(std::cin, start_k, end_k, separator);
    }
    else {
        std::ifstream f(name);
        sort_k_t(f, start_k, end_k, separator);
    }
    return 0;
}