#include "Percolation.h"

#include <cstdlib>

Percolation::Percolation(std::size_t dimension)
    : data(std::vector<std::vector<std::size_t>>(dimension))
{
    for (std::size_t j = 0; j < dimension; j++) {
        data[j] = std::vector(dimension, static_cast<std::size_t>(0));
    }
}
size_t Percolation::get_marker(std::size_t row, std::size_t column)
{
    if (row == data.size()) {
        return 0;
    }
    if (column == data.size()) {
        return 0;
    }
    return data[row][column];
}
size_t Percolation::mark_as_neighbour(std::size_t row, std::size_t column)
{
    if (row > 0 && get_marker(row - 1, column) != 0) {
        return get_marker(row - 1, column);
    }
    if (column > 0 && get_marker(row, column - 1) != 0) {
        return get_marker(row, column - 1);
    }
    if (get_marker(row + 1, column) != 0) {
        return get_marker(row + 1, column);
    }
    if (get_marker(row, column + 1) != 0) {
        return get_marker(row, column + 1);
    }
    return marker++;
}
void Percolation::open(size_t row, size_t column)
{
    if (data[row][column] != 0)
        return;
    data[row][column] = mark_as_neighbour(row, column);
    container.union_percolation_markers(data[row][column], row, column, data);
    open_number++;
}

bool Percolation::is_open(size_t row, size_t column) const { return data[row][column] != 0; }
bool Percolation::is_full(size_t row, size_t column) const { return container.is_full(data[row][column]) != 0; }
size_t Percolation::get_numbet_of_open_cells() const { return open_number; }
bool Percolation::has_percolation() const { return container.has_percolation_variable; }
