package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Event;
import ru.itmo.wp.model.domain.Type;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.EventService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused"})
public class LogoutPage extends Page {
    private final EventService eventRepository = new EventService();
    private final UserService userService = new UserService();

    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        eventRepository.save(new Event(Type.LOGOUT, ((User) request.getSession().getAttribute("user")).getId()));
        request.getSession().removeAttribute("user");
        request.getSession().setAttribute("message", "Good bye. Hope to see you soon!");
        throw new RedirectException("/index");
    }
}
