package ru.itmo.wp.service;

import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.repository.TagRepository;

public class TagService {
    private final TagRepository tagRepository;

    public TagService(TagRepository tagRepository) {
        this.tagRepository = tagRepository;
    }

    public Tag findByName(String name) {
        return name == null ? null : tagRepository.findTagByName(name);
    }

}
