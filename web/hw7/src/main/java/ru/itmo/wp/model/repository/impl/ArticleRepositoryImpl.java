package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.service.UserService;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ArticleRepositoryImpl extends AbstractRepositoryImpl implements ArticleRepository {

    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    private final UserService userService = new UserService();

    @Override
    public Article findBy(String keyType, Object key) {
        return (Article) super.findBy(keyType, key, "Article");
    }

    @Override
    public List<Article> findAll(boolean onlyAvailable) {
        List<Article> articles = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Article ORDER BY id DESC")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    Article article;
                    while ((article = toCurrentType(statement.getMetaData(), resultSet)) != null) {
                        if (onlyAvailable) {
                            if (article.getAvailability()) {
                                articles.add(article);
                            }
                        } else {
                            articles.add(article);
                        }
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Article.", e);
        }
        return articles;
    }

    @Override
    public List<Article> findAllByUser(long userId) {
        List<Article> articles = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            String request = "SELECT * FROM Article WHERE userId=" + userId + " ORDER BY creationTime DESC";
            try (PreparedStatement statement = connection.prepareStatement(request)) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    Article article;
                    while ((article = toCurrentType(statement.getMetaData(), resultSet)) != null) {
                        articles.add(article);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Talk.", e);
        }
        return articles;
    }

    @Override
    protected Article toCurrentType(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Article article = new Article();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    article.setId(resultSet.getLong(i));
                    break;
                case "userId":
                    article.setUserId(resultSet.getLong(i));
                    break;
                case "creationTime":
                    article.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "text":
                    article.setText(resultSet.getString(i));
                    break;
                case "title":
                    article.setTitle(resultSet.getString(i));
                    break;
                case "hidden":
                    article.setAvailability(resultSet.getBoolean(i));
                    break;
                default:
                    // No operations.
            }
        }
        article.setUserLogin(userService.find(article.getUserId()).getLogin());
        return article;
    }

    @Override
    public void save(Article article) {
        super.save(article, new Object[]{article.getUserId(), article.getTitle(), article.getText(), false},
                "INSERT INTO `Article` (`userId`, `title`, `text`, `hidden`, `creationTime`) VALUES (?, ?, ?, ?, NOW())", "Article");
    }

    @Override
    public void updateHidden(Article article, boolean availability) {
        article.setAvailability(availability);
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("UPDATE Article SET hidden=? WHERE id=?")) {
                statement.setBoolean(1, availability);
                statement.setLong(2, article.getId());
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't update article's availability");
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find article", e);
        }
    }

}
