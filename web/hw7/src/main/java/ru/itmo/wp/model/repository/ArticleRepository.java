package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;

import java.util.List;

public interface ArticleRepository {
    Article findBy(String keyType, Object key);

    List<Article> findAll(boolean onlyAvailable);

    List<Article> findAllByUser(long userId);

    void save(Article article);

    void updateHidden(Article article, boolean availability);
}
