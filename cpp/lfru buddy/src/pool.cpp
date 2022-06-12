#include "pool.h"

std::size_t PoolAllocator::left(std::size_t iterator) { return 2 * iterator + 1; }
std::size_t PoolAllocator::right(std::size_t iterator) { return 2 * iterator + 2; }
std::size_t PoolAllocator::parent(std::size_t iterator) { return (iterator - 1) / 2; }
std::size_t PoolAllocator::buddy(std::size_t iterator) { return iterator % 2 == 0 ? iterator - 1 : iterator + 1; }

std::size_t PoolAllocator::finding_of_empty_block(const std::size_t n)
{
    std::size_t search_size = (n == 1 ? 1 : 1 << (32 - (__builtin_clz(n - 1))));
    search_size = std::max(search_size, m_min_size);
    std::size_t current = 0;
    if (search_size > cache_heap[0]) {
        throw std::bad_alloc{};
    }
    std::size_t result_index = 0;
    std::size_t level_weight = m_max_size;
    alloc_searching(current, level_weight, result_index, search_size);
    resize(current);
    return result_index;
}
void PoolAllocator::preparation()
{
    cache_heap = std::vector(2 * m_max_size / m_min_size - 1, nullptr_for_size_t);
    cache_heap[0] = m_max_size;
}

void PoolAllocator::dealloc_searching(std::size_t & iterator, std::size_t & level_weight, std::size_t searching_index, std::size_t point)
{
    while (left(iterator) < cache_heap.size() && cache_heap[left(iterator)] != nullptr_for_size_t) {
        if (level_weight / 2 + searching_index > point) {
            iterator = left(iterator);
        }
        else {
            searching_index += level_weight / 2;
            iterator = right(iterator);
        }
        level_weight /= 2;
    }
}

void PoolAllocator::blocks_merging(std::size_t & iterator, std::size_t & level_weight)
{
    if (iterator != 0) {
        while (iterator != 0 && cache_heap[buddy(iterator)] == level_weight) {
            iterator = parent(iterator);
            cache_heap[iterator] = level_weight * 2;
            cache_heap[left(iterator)] = nullptr_for_size_t;
            cache_heap[right(iterator)] = nullptr_for_size_t;
            level_weight *= 2;
        }
    }
    cache_heap[iterator] = level_weight;
}

void PoolAllocator::deallocate(const void * ptr)
{
    auto b_ptr = static_cast<const std::byte *>(ptr);
    const auto begin = m_storage.data();
    std::size_t point = (b_ptr - begin);
    std::size_t current = 0;
    std::size_t searching_index = 0;
    std::size_t level_weight = m_max_size;
    dealloc_searching(current, level_weight, searching_index, point);
    blocks_merging(current, level_weight);
    resize(current);
}

void * PoolAllocator::allocate(const std::size_t n) { return &m_storage[finding_of_empty_block(n)]; }

void PoolAllocator::resize(std::size_t current)
{
    while (current != 0) {
        std::size_t max = std::max(cache_heap[current], cache_heap[buddy(current)]);
        std::size_t min = std::min(cache_heap[current], cache_heap[buddy(current)]);
        cache_heap[parent(current)] = (max != nullptr_for_size_t ? max : min);
        current = parent(current);
    }
}

void PoolAllocator::alloc_searching(std::size_t & iterator, std::size_t & level_weight, std::size_t & result_index, std::size_t search_size)
{
    while (level_weight != search_size) {
        if (cache_heap[left(iterator)] == nullptr_for_size_t) {
            cache_heap[left(iterator)] = cache_heap[iterator] / 2;
            cache_heap[right(iterator)] = cache_heap[iterator] / 2;
            iterator = left(iterator);
        }
        else if ((cache_heap[left(iterator)] <= cache_heap[right(iterator)] && cache_heap[left(iterator)] >= search_size) ||
                 cache_heap[right(iterator)] < search_size) {
            iterator = left(iterator);
        }
        else {
            result_index += level_weight / 2;
            iterator = right(iterator);
        }
        level_weight /= 2;
    }
    cache_heap[iterator] = 0;
}
