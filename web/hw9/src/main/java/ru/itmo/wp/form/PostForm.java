package ru.itmo.wp.form;

import javax.persistence.Lob;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

public class PostForm {
    @NotNull
    @NotEmpty
    @Size(min = 1, max = 60)
    private String title;

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    @NotNull
    @NotEmpty
    @Size(min = 1, max = 65000)
    @Lob
    private String text;

    @Pattern(regexp = "\\s*([a-zA-Z]+\\s+)*[a-zA-Z]*\\s*",
            message = "tags must be words with latin letters")
    String tags;

    public String getTags() {
        return tags;
    }

    public void setTags(String tags) {
        this.tags = tags;
    }


}
