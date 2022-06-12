#pragma once

#include <random>
#include <stdio.h>
#include <vector>

struct PercolationStats
{
public:
    PercolationStats(size_t dimension, size_t trials);

    double get_mean() const;

    double get_standard_deviation() const;

    double get_confidence_low() const;

    double get_confidence_high() const;

    void execute();

private:
    struct random_struct
    {
        random_struct()
            : m_rand_engine(std::random_device{}())
        {
        }

    public:
        std::size_t get_rand(std::size_t from, std::size_t to)
        {
            std::uniform_int_distribution distribution(from, to);
            return distribution(m_rand_engine);
        }

    private:
        mutable std::mt19937 m_rand_engine;
    };
    std::pair<std::size_t, std::size_t> random(std::size_t open_number);
    void remake_random_vector();
    std::vector<std::vector<std::size_t>> random_vector;
    std::vector<double> results;
    double x = 0;
    double s = 0;
    size_t size = 0;
    size_t count_of_repeats = 0;
    random_struct randomizer;
};
