package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.HasDateAndId;
import ru.itmo.wp.model.exception.RepositoryException;

import javax.sql.DataSource;
import java.sql.*;

public abstract class AbstractRepositoryImpl {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    public void save(HasDateAndId item, Object[] params, String request, String name) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    request,
                    Statement.RETURN_GENERATED_KEYS
            )) {
                for (int i = 0; i < params.length; i++) {
                    if (params[i] instanceof Long) {
                        statement.setLong(i + 1, Long.parseLong(String.valueOf(params[i])));
                    }
                    if (params[i] instanceof String) {
                        statement.setString(i + 1, String.valueOf(params[i]));
                    }
                }
                if (statement.executeUpdate() != 1) {
                    throw new RepositoryException("Can't save");
                } else {
                    ResultSet generatedKeys = statement.getGeneratedKeys();
                    if (generatedKeys.next()) {
                        item.setId(generatedKeys.getLong(1));
                        item.setCreationTime(findBy("id", item.getId(), name).getCreationTime());
                    } else {
                        throw new RepositoryException("Can't save [no autogenerated fields].");
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't save ", e);
        }
    }

    public HasDateAndId findBy(String keyType, Object key, String name) {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            String request = "SELECT * FROM " + name + " WHERE " + keyType + "=?";
            try (PreparedStatement statement = connection.prepareStatement(request)) {
                if (key.getClass().equals(String.class)) {
                    statement.setString(1, String.valueOf(key));
                } else if (key.getClass().equals(Long.class)) {
                    statement.setLong(1, (Long) key);
                }
                try (ResultSet resultSet = statement.executeQuery()) {
                    return toCurrentType(statement.getMetaData(), resultSet);
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find", e);
        }
    }

    protected abstract HasDateAndId toCurrentType(ResultSetMetaData resultSetMetaData, ResultSet resultSet) throws SQLException;

}
