package ru.itmo.wp.model.domain;

import java.util.Date;

public class Talk extends AbstractClassWithIdAndCreationTime {
    private User sourceUser;
    private User targetUser;
    private long sourceUserId;
    private long targetUserId;
    private String text;

    public Talk() {
    }

    public Talk(long sourceUserId, long targetUserid, String text) {
        this.sourceUserId = sourceUserId;
        this.targetUserId = targetUserid;
        this.text = text;
    }

    public User getSourceUser() {
        return sourceUser;
    }

    public void setSourceUser(User sourceUser) {
        this.sourceUser = sourceUser;
    }

    public User getTargetUser() {
        return targetUser;
    }

    public void setTargetUser(User targetUser) {
        this.targetUser = targetUser;
    }

    public long getTargetUserId() {
        return targetUserId;
    }

    public void setTargetUserId(long targetUserId) {
        this.targetUserId = targetUserId;
    }

    public long getId() {
        return super.getId();
    }

    public void setId(long id) {
        super.setId(id);
    }

    public long getSourceUserId() {
        return sourceUserId;
    }

    public void setSourceUserId(long sourceUserId) {
        this.sourceUserId = sourceUserId;
    }

    public long getTargetUserid() {
        return targetUserId;
    }

    public void setTargetUserid(long targetUserid) {
        this.targetUserId = targetUserid;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public Date getCreationTime() {
        return super.getCreationTime();
    }

    public void setCreationTime(Date creationTime) {
        super.setCreationTime(creationTime);
    }


}
