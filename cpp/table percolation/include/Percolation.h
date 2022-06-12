#pragma once

#include <stdio.h>
#include <vector>

struct Percolation
{
    Percolation(size_t dimension);

    void open(size_t row, size_t column);

    bool is_open(size_t row, size_t column) const;

    bool is_full(size_t row, size_t column) const;

    bool has_percolation() const;

    size_t get_numbet_of_open_cells() const;

private:
    struct DSU
    {
        void union_percolation_markers(std::size_t data_marker, std::size_t row, std::size_t column, const std::vector<std::vector<std::size_t>> & current_massive)
        {
            if (row != 0)
                simple_union(data_marker, current_massive[row - 1][column], row, current_massive);
            if (row != current_massive.size() - 1)
                simple_union(data_marker, current_massive[row + 1][column], row, current_massive);
            if (column != 0)
                simple_union(data_marker, current_massive[row][column - 1], row, current_massive);
            if (column != current_massive.size() - 1)
                simple_union(data_marker, current_massive[row][column + 1], row, current_massive);
        }
        void simple_union(std::size_t last_opened, std::size_t neighbour, std::size_t row, const std::vector<std::vector<std::size_t>> & current_massive)
        {
            if (std::min(last_opened, neighbour) == roots.size())
                roots.push_back(std::min(last_opened, neighbour));
            if (std::max(last_opened, neighbour) == roots.size())
                roots.push_back(std::max(last_opened, neighbour));
            if (std::min(last_opened, neighbour) == ranks.size())
                ranks.push_back(1);
            if (std::max(last_opened, neighbour) == ranks.size())
                ranks.push_back(1);
            if (last_opened != 0) {
                if (neighbour != 0 && map.size() > get(last_opened) && map.size() > get(neighbour) && (map[get(last_opened)] + map[get(neighbour)] == 3))
                    has_percolation_variable = true;
                if (map.size() > (get(last_opened)) && ((map[get(last_opened)] == 1 && row == current_massive.size() - 1) || (map[get(last_opened)] == 2 && row == 0)))
                    has_percolation_variable = true;
                if (neighbour != 0 && map.size() > (get(neighbour)) && ((map[get(neighbour)] == 1 && row == current_massive.size() - 1) || (map[get(neighbour)] == 2 && row == 0)))
                    has_percolation_variable = true;
                if (map.size() > (get(last_opened))) {
                    if (row == 0 || row == current_massive.size() - 1) {
                        map[get(last_opened)] = row == 0 ? 1 : 2;
                    }
                }
                else {
                    while (map.size() <= get(last_opened)) {
                        map.push_back(0);
                    }
                    if (row == 0) {
                        map[get(last_opened)] = 1;
                    }
                    else if (row == current_massive.size() - 1) {
                        map[get(last_opened)] = 2;
                    }
                    else
                        map[get(last_opened)] = 0;
                }
                if (neighbour != 0) {
                    last_opened = get(last_opened);
                    neighbour = get(neighbour);
                    if (last_opened == neighbour) {
                        return;
                    }
                    if (ranks[last_opened] == ranks[neighbour]) {
                        ranks[last_opened]++;
                    }
                    ranks[last_opened] < ranks[neighbour] ? repercolate(neighbour, last_opened) : repercolate(last_opened, neighbour);
                }
            }
        }
        int is_full(std::size_t element) const
        {
            std::size_t root = get_without_rebalance(element);
            if (map.size() > (root)) {
                return map[root];
            }
            return 0;
        }
        void repercolate(std::size_t parent, std::size_t child)
        {
            if (map[roots[child]] != 0) {
                map[roots[parent]] = map[roots[child]];
            }
            roots[child] = parent;
        }

        bool has_percolation_variable = false;
        std::vector<std::size_t> map;

    private:
        std::size_t get_without_rebalance(std::size_t element) const
        {
            std::size_t root = element;
            while (roots[root] != root) {
                root = roots[root];
            }
            return root;
        }
        std::size_t get(std::size_t element)
        {
            std::size_t root = get_without_rebalance(element);
            std::size_t i = element;
            while (roots[i] != i) {
                std::size_t j = roots[i];
                roots[i] = root;
                i = j;
            }
            return root;
        }
        std::vector<size_t> roots = {0};
        std::vector<size_t> ranks = {0};
    };
    std::vector<std::vector<size_t>> data;
    std::size_t get_marker(std::size_t row, std::size_t column);
    std::size_t mark_as_neighbour(std::size_t row, std::size_t column);
    std::size_t open_number = 0;
    std::size_t marker = 1;
    DSU container;
};
