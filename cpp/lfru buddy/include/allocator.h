#pragma once

#include "pool.h"

class AllocatorWithPool : private PoolAllocator
{
public:
    AllocatorWithPool(const unsigned min_p, const unsigned max_p)
        : PoolAllocator(min_p, max_p)
    {
    }

    template <class T, class... Args>
    T * create(Args &&... args)
    {
        auto * ptr = allocate(sizeof(T));
        return new (ptr) T(std::forward<Args>(args)...);
    }

    template <class T>
    void destroy(void * ptr)
    {
        static_cast<T *>(ptr)->~T();
        deallocate(ptr);
    }
};
