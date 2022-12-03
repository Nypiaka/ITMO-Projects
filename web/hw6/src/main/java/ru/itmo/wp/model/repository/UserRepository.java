package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.User;

import java.util.List;

public interface UserRepository {
    User findBy(String keyType, Object key);

    User findBySmthAndPasswordSha(Object smth, String smthType, String passwordSha);

    List<User> findAll();

    void save(User user, String passwordSha);

    long findCount();

}
