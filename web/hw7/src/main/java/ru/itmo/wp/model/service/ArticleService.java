package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImpl;

import java.util.List;

public class ArticleService {
    private final ArticleRepository articleRepository = new ArticleRepositoryImpl();

    public void validateArticle(Article article) throws ValidationException {
        if (article.getText().isBlank() || Strings.isNullOrEmpty(article.getText())) {
            throw new ValidationException("Text is Empty");
        }
        if (article.getTitle().isBlank() || Strings.isNullOrEmpty(article.getText())) {
            throw new ValidationException("Title is Empty");
        }
        if (article.getTitle().length() > 255) {
            throw new ValidationException("Title is too long (255 max)");
        }
    }

    public void save(Article article) {
        articleRepository.save(article);
    }

    public List<Article> findAll() {
        return articleRepository.findAll(false);
    }

    public Article find(long id) {
        return articleRepository.findBy("id", id);
    }

    public Article findBy(String keyType, Object key) {
        return articleRepository.findBy(keyType, key);
    }

    public List<Article> findAllByUser(long userId) {
        return articleRepository.findAllByUser(userId);
    }

    public void updateHidden(Article article, boolean availability) {
        articleRepository.updateHidden(article, availability);
    }

    public List<Article> findAllAvailable() {
        return articleRepository.findAll(true);
    }

    public void validateArticleAndUser(User sessionUser, Article article) throws ValidationException {
        if (sessionUser.getId() != article.getUserId()) {
            throw new ValidationException("The current user has no right to change the availability of this article");
        }
    }

    public void validateArticleAndButton(Article article, String buttonText) throws ValidationException {
        if (!((article.getAvailability() && buttonText.equals("Hide")) || (!article.getAvailability() && buttonText.equals("Show")))) {
            throw new ValidationException("Wrong button text");
        }
    }
}

