package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/**
 * @noinspection unused
 */
public class UsersPage {
    private void action(HttpServletRequest request, Map<String, Object> view) {
    }

    private final UserService userService = new UserService();

    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("users", userService.findAll());
    }

    private void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("user",
                userService.find(Long.parseLong(request.getParameter("userId"))));
    }

    private void changeAdmin(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User sessionUser = (User) request.getSession().getAttribute("user");

        String userId = request.getParameter("changeAdminId");

        String buttonText = request.getParameter("buttonText");


        User userToChange = userService.find(Long.parseLong(userId));

        userService.validateUser(userToChange);

        if (buttonText == null) {
            request.getSession().setAttribute("message", "Button text required");
            throw new RedirectException("/index");
        }

        userService.validateAdminChangeAndUser(sessionUser);

        userService.validateAdminChangeAndButton(userToChange, buttonText);

        userService.updateAdminRights(userToChange, buttonText.equals("enable"));
        if (Long.parseLong(userId) == sessionUser.getId()) {
            request.getSession().setAttribute("user", userService.find(sessionUser.getId()));
        }
    }
}
