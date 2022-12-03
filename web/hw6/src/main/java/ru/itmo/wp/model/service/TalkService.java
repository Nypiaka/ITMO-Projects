package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.repository.impl.TalkRepositoryImpl;

import java.util.List;

public class TalkService {
    private final TalkRepositoryImpl talkRepository = new TalkRepositoryImpl();


    public List<Talk> findAll(long id) {
        return talkRepository.findAll(id);
    }

    public void save(Talk talk) {
        talkRepository.save(talk);
    }

    public void validateUser(User user, String userType) throws ValidationException {
        if (user == null) {
            throw new ValidationException("Unknown " + userType);
        }
    }

    public void validateText(String text) throws ValidationException {
        if (Strings.isNullOrEmpty(text) || text.isBlank()) {
            throw new ValidationException("Text is required");
        }
        if (text.length() > 255) {
            throw new ValidationException("Text is too long. Max 255.");
        }
    }
}
