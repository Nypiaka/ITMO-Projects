#pragma once
#include <functional>
#include <list>
#include <vector>
class PoolAllocator
{
public:
    PoolAllocator(const unsigned min_p, const unsigned max_p)
        : m_storage(1 << max_p)
        , m_min_size(1 << min_p)
        , m_max_size(1 << max_p)
    {
        preparation();
    }

    void * allocate(const std::size_t n);
    void deallocate(const void * ptr);

private:
    void preparation();
    void dealloc_searching(std::size_t & iterator, std::size_t & level_weight, std::size_t searching_index, std::size_t point);
    void blocks_merging(std::size_t & iterator, std::size_t & level_weight);
    void alloc_searching(std::size_t & iterator, std::size_t & level_weight, std::size_t & result_index, std::size_t search_size);
    void resize(std::size_t current);
    std::size_t left(std::size_t iterator);
    std::size_t right(std::size_t iterator);
    std::size_t parent(std::size_t iterator);
    std::size_t buddy(std::size_t iterator);
    std::size_t finding_of_empty_block(const std::size_t n);
    std::vector<std::byte> m_storage;
    std::vector<std::size_t> cache_heap;
    std::size_t m_min_size;
    std::size_t m_max_size;
    const std::size_t nullptr_for_size_t = static_cast<std::size_t>(-1);
};
