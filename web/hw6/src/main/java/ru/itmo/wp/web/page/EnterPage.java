package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.Type;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.EventService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused"})
public class EnterPage extends Page {
    private final UserService userService = new UserService();

    private final EventService eventRepository = new EventService();

    private void enter(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        String loginOrEmail = request.getParameter("loginOrEmail");
        String password = request.getParameter("password");

        userService.validateEnter(loginOrEmail, password);
        User user = null;
        for (String currentKey : new String[]{"login", "email"}) {
            if (user == null)
                user = userService.findBySmthAndPassword(loginOrEmail, currentKey, password);
        }

        request.getSession().setAttribute("user", user);
        request.getSession().setAttribute("message", "Hello, " + user.getLogin());
        eventRepository.save(new Event(Type.ENTER, user.getId()));
        throw new RedirectException("/index");
    }
}
