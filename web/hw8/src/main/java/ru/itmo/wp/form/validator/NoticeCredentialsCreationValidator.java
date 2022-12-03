package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import ru.itmo.wp.form.NoticeCredentials;

import org.springframework.validation.Validator;

@Component
public class NoticeCredentialsCreationValidator implements Validator {

    public boolean supports(Class<?> clazz) {
        return NoticeCredentials.class.equals(clazz);
    }

    public void validate(Object target, Errors errors) {
    }
}
