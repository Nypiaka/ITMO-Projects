#pragma once
#include <algorithm>
#include <cstddef>
#include <new>
#include <ostream>

template <class Key, class KeyProvider, class Allocator>
class Cache
{
public:
    template <class... AllocArgs>
    Cache(const std::size_t cache_size, AllocArgs &&... alloc_args)
        : m_max_top_size(cache_size)
        , m_max_low_size(cache_size)
        , m_alloc(std::forward<AllocArgs>(alloc_args)...)
    {
    }

    std::size_t size() const
    {
        return m_queue_low_priority.size() + m_queue_high_priority.size();
    }

    bool empty() const
    {
        return m_queue_low_priority.size() + m_queue_high_priority.size() == 0;
    }

    template <class T>
    T & get(const Key & key);

    std::ostream & print(std::ostream & strm) const;

    friend std::ostream & operator<<(std::ostream & strm, const Cache & cache)
    {
        return cache.print(strm);
    }

private:
    const std::size_t m_max_top_size;
    const std::size_t m_max_low_size;
    std::list<KeyProvider *> m_queue_high_priority;
    std::list<KeyProvider *> m_queue_low_priority;
    Allocator m_alloc;
};

template <class Key, class KeyProvider, class Allocator>
template <class T>
inline T & Cache<Key, KeyProvider, Allocator>::get(const Key & key)
{
    auto high_priority_queue_operator = std::find_if(m_queue_high_priority.begin(), m_queue_high_priority.end(), [&key](const KeyProvider * ptr) {
        return *ptr == key;
    });
    if (high_priority_queue_operator != m_queue_high_priority.end()) {
        m_queue_high_priority.splice(m_queue_high_priority.begin(), m_queue_high_priority, high_priority_queue_operator);
    }
    else {
        auto low_priority_queue_operator = std::find_if(m_queue_low_priority.begin(), m_queue_low_priority.end(), [&key](const KeyProvider * ptr) {
            return *ptr == key;
        });
        if (low_priority_queue_operator != m_queue_low_priority.end()) {
            auto copy_of_iterator = *low_priority_queue_operator;
            m_queue_low_priority.erase(low_priority_queue_operator);
            if (m_queue_high_priority.size() == m_max_top_size) {
                m_queue_low_priority.push_front(m_queue_high_priority.back());
                m_queue_high_priority.pop_back();
            }
            m_queue_high_priority.push_front(copy_of_iterator);
        }
        else {
            if (m_queue_low_priority.size() == m_max_low_size) {
                m_alloc.template destroy<KeyProvider>(m_queue_low_priority.back());
                m_queue_low_priority.pop_back();
            }
            m_queue_low_priority.push_front((m_alloc.template create<T>(key)));
            return *static_cast<T *>(m_queue_low_priority.front());
        }
    }
    return *static_cast<T *>(m_queue_high_priority.front());
}

template <class Key, class KeyProvider, class Allocator>
inline std::ostream & Cache<Key, KeyProvider, Allocator>::print(std::ostream & strm) const
{
    bool first = true;
    for (const auto ptr : m_queue_high_priority) {
        if (!first) {
            strm << " ";
        }
        else {
            first = false;
        }
        strm << *ptr;
    }
    strm << "\n";
    for (const auto ptr : m_queue_low_priority) {
        if (!first) {
            strm << " ";
        }
        else {
            first = false;
        }
        strm << *ptr;
    }
    strm << "\n";
    return strm;
}
