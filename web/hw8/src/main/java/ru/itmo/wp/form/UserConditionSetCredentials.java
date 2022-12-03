package ru.itmo.wp.form;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

public class UserConditionSetCredentials {
    private long userId;
    @NotEmpty
    @NotNull
    @Pattern(regexp = "^(Enable|Disable)$", message = "Masha dont break this button! >:(")
    private String status;

    public long getUserId() {
        return userId;
    }

    public void setUserId(long userId) {
        this.userId = userId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

}
