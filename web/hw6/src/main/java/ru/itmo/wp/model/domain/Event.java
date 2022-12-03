package ru.itmo.wp.model.domain;

import java.util.Date;

public class Event extends AbstractClassWithIdAndCreationTime {
    private long userId;
    private Type type;

    public Event() {
    }

    public Event(Type type, long userId) {
        this.type = type;
        this.userId = userId;
    }

    public long getId() {
        return super.getId();
    }

    public void setId(long id) {
        super.setId(id);
    }

    public long getUserId() {
        return userId;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }


    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }
}
