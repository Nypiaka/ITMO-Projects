package ru.itmo.wp.model.domain;

import java.io.Serializable;
import java.util.Date;

public class User extends AbstractClassWithIdAndCreationTime implements Serializable {
    private String login;
    private String email;

    public long getId() {
        return super.getId();
    }

    public void setId(long id) {
        super.setId(id);
    }

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public Date getCreationTime() {
        return super.getCreationTime();
    }

    public void setCreationTime(Date creationTime) {
        super.setCreationTime(creationTime);
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
}
