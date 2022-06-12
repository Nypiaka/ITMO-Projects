#include "PercolationStats.h"

#include "Percolation.h"

#include <cmath>

PercolationStats::PercolationStats(size_t dimension, size_t trials)
    : random_vector(std::vector<std::vector<std::size_t>>(2))
    , size(dimension)
    , count_of_repeats(trials)

{
    execute();
}

double PercolationStats::get_mean() const { return x; }
double PercolationStats::get_standard_deviation() const { return sqrt(s); }
double PercolationStats::get_confidence_low() const { return (x - 1.96 * sqrt(s) / sqrt(static_cast<double>(count_of_repeats))); }
double PercolationStats::get_confidence_high() const { return (x + 1.96 * sqrt(s) / sqrt(static_cast<double>(count_of_repeats))); }

void PercolationStats::remake_random_vector()
{
    random_vector[0].resize(size * size);
    random_vector[1].resize(size * size);
    std::size_t k = 0;
    for (std::size_t i = 0; i < size; i++) {
        for (std::size_t j = 0; j < size; j++) {
            random_vector[0][k] = i;
            random_vector[1][k] = j;
            k++;
        }
    }
}

std::pair<std::size_t, std::size_t> PercolationStats::random(std::size_t open_number)
{
    std::size_t res = randomizer.get_rand(0, size * size - open_number - 1);
    std::pair<std::size_t, std::size_t> result = std::pair(random_vector[0][res], random_vector[1][res]);
    random_vector[0].erase(random_vector[0].begin() + res);
    random_vector[1].erase(random_vector[1].begin() + res);
    return result;
}

void PercolationStats::execute()
{
    results = std::vector<double>(count_of_repeats);
    for (std::size_t i = 0; i < count_of_repeats; i++) {
        Percolation maker(size);
        remake_random_vector();
        while (!maker.has_percolation()) {
            std::pair<std::size_t, std::size_t> r = random(maker.get_numbet_of_open_cells());
            maker.open(r.first, r.second);
            if (size == 1) {
                break;
            }
        }
        results[i] = static_cast<double>(maker.get_numbet_of_open_cells()) / static_cast<double>(size * size);
        x += results[i] / static_cast<double>(count_of_repeats);
    }
    for (const double result : results) {
        s += (result - x) * (result - x);
    }
    s /= static_cast<double>(count_of_repeats - 1);
}
