package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.service.UserService;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class TalkRepositoryImpl extends AbstractRepositoryImpl implements TalkRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    private final UserService userService = new UserService();

    @Override
    public void save(Talk talk) {
        super.save(talk, new Object[]{talk.getSourceUserId(), talk.getTargetUserid(), talk.getText()},
                "INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `text`, `creationTime`) VALUES (?, ?, ?, NOW())", "Talk");
    }


    public Talk findBy(String keyType, Object key) {
        return (Talk) super.findBy(keyType, key, "Talk");
    }

    @Override
    protected Talk toCurrentType(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Talk talk = new Talk();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    talk.setId(resultSet.getLong(i));
                    break;
                case "creationTime":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "text":
                    talk.setText(resultSet.getString(i));
                    break;
                case "sourceUserId":
                    talk.setSourceUserId(resultSet.getLong(i));
                    break;
                case "targetUserId":
                    talk.setTargetUserid(resultSet.getLong(i));
                    break;
                default:
                    // No operations.
            }
        }

        return talk;
    }

    public List<Talk> findAll(Long userId) {
        List<Talk> talks = new ArrayList<>();
        try (Connection connection = DATA_SOURCE.getConnection()) {
            String request = "SELECT * FROM Talk WHERE sourceUserId=" + userId.toString() + " OR targetUserId=" + userId;
            try (PreparedStatement statement = connection.prepareStatement(request)) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    Talk talk;
                    while ((talk = toCurrentType(statement.getMetaData(), resultSet)) != null) {
                        talk.setTargetUser(userService.findBy("id", talk.getTargetUserid()));
                        talk.setSourceUser(userService.findBy("id", talk.getSourceUserId()));
                        talks.add(talk);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find Talk.", e);
        }
        return talks;
    }
}
