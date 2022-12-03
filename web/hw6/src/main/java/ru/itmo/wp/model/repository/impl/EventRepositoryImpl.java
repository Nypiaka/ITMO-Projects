package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.database.DatabaseUtils;
import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.Type;
import ru.itmo.wp.model.repository.EventRepository;

import javax.sql.DataSource;
import java.sql.*;

public class EventRepositoryImpl extends AbstractRepositoryImpl implements EventRepository {
    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();

    @Override
    public void save(Event event) {
        super.save(event, new Object[]{event.getUserId(), event.getType().name()},
                "INSERT INTO `Event` (`userId`, `type`, `creationTime`) VALUES (?, ?, NOW())", "Event");
    }

    public Event findBy(String keyType, Object key) {
        return (Event) super.findBy(keyType, key, "Event");
    }

    @Override
    protected Event toCurrentType(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Event event = new Event();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    event.setId(resultSet.getLong(i));
                    break;
                case "creationTime":
                    event.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "type":
                    event.setType(Type.valueOf(resultSet.getString(i)));
                    break;
                default:
                    // No operations.
            }
        }

        return event;
    }

}
