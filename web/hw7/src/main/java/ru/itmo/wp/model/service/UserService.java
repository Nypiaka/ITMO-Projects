package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import com.google.common.hash.Hashing;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * @noinspection UnstableApiUsage
 */
public class UserService {
    private final UserRepository userRepository = new UserRepositoryImpl();
    private static final String PASSWORD_SALT = "160e65dc7185ae4a6effd0402a32c33f9f393779";

    public void validateRegistration(User user, String password) throws ValidationException {
        if (Strings.isNullOrEmpty(user.getLogin())) {
            throw new ValidationException("Login is required");
        }
        if (!user.getLogin().matches("[a-z]+")) {
            throw new ValidationException("Login can contain only lowercase Latin letters");
        }
        if (user.getLogin().length() > 20) {
            throw new ValidationException("Login can't be longer than 20 letters");
        }
        if (userRepository.findBy("login", user.getLogin()) != null) {
            throw new ValidationException("Login is already in use");
        }

        if (Strings.isNullOrEmpty(password)) {
            throw new ValidationException("Password is required");
        }
        if (password.length() < 4) {
            throw new ValidationException("Password can't be shorter than 4 characters");
        }
        if (password.length() > 25) {
            throw new ValidationException("Password can't be longer than 25 characters");
        }
    }

    public void register(User user, String password) {
        userRepository.save(user, getPasswordSha(password));
    }

    private String getPasswordSha(String password) {
        return Hashing.sha256().hashBytes((PASSWORD_SALT + password).getBytes(StandardCharsets.UTF_8)).toString();
    }

    public List<User> findAll() {
        return userRepository.findAll();
    }

    public User find(long id) {
        return userRepository.findBy("id", id);
    }

    public User validateAndFindByLoginAndPassword(String login, String password) {
        return userRepository.findByLoginAndPasswordSha(login, getPasswordSha(password));

    }

    public void validateUser(User user) throws ValidationException {
        if (user == null) {
            throw new ValidationException("Invalid user");
        }
    }

    public void validateAdminChangeAndUser(User user) throws ValidationException {
        if (!userRepository.findBy("id", user.getId()).getAdmin()) {
            throw new ValidationException("User cant change permissions");
        }
    }

    public void validateAdminChangeAndButton(User user, String buttonText) throws ValidationException {
        if (!((user.getAdmin() && buttonText.equals("disable")) || (!user.getAdmin() && buttonText.equals("enable")))) {
            throw new ValidationException("Wrong button text");
        }
    }

    public void updateAdminRights(User userToChange, boolean enable) {
        userRepository.updateAdminRights(userToChange, enable);
    }
}
