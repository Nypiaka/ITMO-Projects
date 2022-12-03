package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import com.google.common.hash.Hashing;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.repository.impl.UserRepositoryImpl;

import java.nio.charset.StandardCharsets;
import java.util.List;

public class UserService {
    private static final String PASSWORD_SALT = "1174f9d7bc21e00e9a5fd0a783a44c9a9f73413c";

    private final UserRepository userRepository = new UserRepositoryImpl();

    public void validateRegistration(User user, String password, String passwordConfirmation) throws ValidationException {
        if (Strings.isNullOrEmpty(user.getLogin())) {
            throw new ValidationException("Login is required");
        }
        if (!user.getLogin().matches("[a-z]+")) {
            throw new ValidationException("Login can contain only lowercase Latin letters");
        }
        if (user.getLogin().length() > 8) {
            throw new ValidationException("Login can't be longer than 8 letters");
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
        if (password.length() > 64) {
            throw new ValidationException("Password can't be longer than 64 characters");
        }
        if (!password.equals(passwordConfirmation)) {
            throw new ValidationException("Passwords aren't the same");
        }
        if (Strings.isNullOrEmpty(user.getEmail())) {
            throw new ValidationException("Email is empty");
        }
        if (userRepository.findBy("email", user.getEmail()) != null) {
            throw new ValidationException("Email is already in use");
        }
        if (user.getEmail().length() > 64 || (!user.getEmail().contains("@") || user.getEmail().length() - user.getEmail().replace("@", "").length() != 1)) {
            throw new ValidationException("Incorrect email");
        }

    }

    public User findBy(String keyType, Object key) {
        return userRepository.findBy(keyType, key);
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

    public void validateEnter(String loginOrEmail, String password) throws ValidationException {
        User user = null;
        for (String currentKey : new String[]{"login", "email"}) {
            if (user == null) {
                user = userRepository.findBySmthAndPasswordSha(loginOrEmail, currentKey, getPasswordSha(password));
            }
        }
        if (user == null) {
            throw new ValidationException("Invalid login or email or password");
        }
    }

    public User findBySmthAndPassword(String smth, String smthType, String password) {
        return userRepository.findBySmthAndPasswordSha(smth, smthType, getPasswordSha(password));
    }

    public long findCount() {
        return userRepository.findCount();
    }
}
