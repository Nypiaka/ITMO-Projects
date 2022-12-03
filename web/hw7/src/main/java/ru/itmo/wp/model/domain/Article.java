package ru.itmo.wp.model.domain;

import java.util.Date;

public class Article extends AbstractClassWithIdAndCreationTime {
    private long userId;
    private String text;
    private String title;

    private boolean availability;
    private String userLogin;

    public Article(long userId, String title, String text) {
        this.userId = userId;
        this.title = title;
        this.text = text;
    }

    public Article() {
    }

    public boolean getAvailability() {
        return availability;
    }

    public void setAvailability(boolean availability) {
        this.availability = availability;
    }

    public String getUserLogin() {
        return userLogin;
    }

    public void setUserLogin(String login) {
        this.userLogin = login;
    }

    public long getUserId() {
        return userId;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public long getId() {
        return super.getId();
    }

    public void setId(long id) {
        super.setId(id);
    }

    public Date getCreationTime() {
        return super.getCreationTime();
    }

    public void setCreationTime(Date creationTime) {
        super.setCreationTime(creationTime);
    }


}
