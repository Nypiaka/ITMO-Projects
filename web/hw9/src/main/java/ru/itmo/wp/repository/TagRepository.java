package ru.itmo.wp.repository;

import org.springframework.data.jpa.repository.JpaRepository;

import ru.itmo.wp.domain.Tag;

public interface TagRepository extends JpaRepository<Tag, Long> {
    Tag findTagByName(String name);

}
