package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.UserRepository;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class UserRepositoryImpl extends AbstractRepositoryImpl implements UserRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    @Override
    public User findBy(String keyType, Object key) {
        return (User) super.findBy(keyType, key, "User");
    }

    @Override
    public User findByLoginAndPasswordSha(String login, String passwordSha) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE login=? AND passwordSha=?")) {
                statement.setString(1, login);
                statement.setString(2, passwordSha);
                try (ResultSet resultSet = statement.executeQuery()) {
                    return toCurrentType(statement.getMetaData(), resultSet);
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
    }

    @Override
    public List<User> findAll() {
        List<User> users = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User ORDER BY id DESC")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    User user;
                    while ((user = toCurrentType(statement.getMetaData(), resultSet)) != null) {
                        users.add(user);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User.", e);
        }
        return users;
    }

    @Override
    protected User toCurrentType(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        User user = new User();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    user.setId(resultSet.getLong(i));
                    break;
                case "login":
                    user.setLogin(resultSet.getString(i));
                    break;
                case "creationTime":
                    user.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "admin":
                    user.setAdmin(resultSet.getBoolean(i));
                default:
                    // No operations.
            }
        }

        return user;
    }

    @Override
    public void save(User user, String passwordSha) {
        super.save(user, new Object[]{user.getLogin(), passwordSha},
                "INSERT INTO `User` (`login`, `passwordSha`, `creationTime`) VALUES (?, ?, NOW())", "User");
    }

    @Override
    public void updateAdminRights(User user, boolean isAdmin) {
        user.setAdmin(isAdmin);
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("UPDATE User SET admin=? WHERE id=?")) {
                statement.setBoolean(1, isAdmin);
                statement.setLong(2, user.getId());
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't update user's rights");
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find user", e);
        }
    }
}
