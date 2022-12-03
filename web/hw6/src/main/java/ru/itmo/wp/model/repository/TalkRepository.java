package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Talk;

public interface TalkRepository {
    void save(Talk talk);
}
