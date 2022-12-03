package ru.itmo.wp.model.domain;

import java.util.Date;

public interface HasDateAndId {
    public void setId(long id);

    public void setCreationTime(Date date);

    public long getId();

    public Date getCreationTime();
}
