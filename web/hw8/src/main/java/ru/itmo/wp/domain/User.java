package ru.itmo.wp.domain;

import org.hibernate.annotations.CreationTimestamp;

import javax.persistence.*;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.Date;

@Entity
@Table(
        indexes = @Index(columnList = "creationTime"),
        uniqueConstraints = @UniqueConstraint(columnNames = "login")
)
public class User implements Serializable {
    @Id
    @GeneratedValue
    private long id;

    @NotNull
    @NotEmpty
    @Size(min = 2, max = 16)
    @Pattern(regexp = "[a-z]+", message = "Only lowercase latin letters expected")
    private String login;

    @CreationTimestamp
    private Date creationTime;

    private boolean status = true;

    public long getId() {
        return id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public String getLogin() {
        return login;
    }

    public void setLogin(String login) {
        this.login = login;
    }

    public Date getCreationTime() {
        return creationTime;
    }

    public void setCreationTime(Date creationTime) {
        this.creationTime = creationTime;
    }

    public boolean getStatus() {
        return status;
    }

    public void setStatus(boolean condition) {
        this.status = condition;
    }
}

