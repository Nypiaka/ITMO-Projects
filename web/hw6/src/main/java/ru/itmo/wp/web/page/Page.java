package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.UserService;
import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public abstract class Page {
    private final UserService userService = new UserService();
    private HttpServletRequest request;

    protected void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    protected void before(HttpServletRequest request, Map<String, Object> view) {
        this.request = request;
        view.put("userCount", userService.findCount());
        User user = (User) request.getSession().getAttribute("user");
        if (user != null) {
            view.put("user", user);
        }
        String message = (String) request.getSession().getAttribute("message");
        if (message != null) {
            view.put("message", message);
            request.getSession().removeAttribute("message");
        }
    }


    protected void after(HttpServletRequest request, Map<String, Object> view) {
    }

    public void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

    public User getUser() {
        return (User) request.getSession().getAttribute("user");
    }

    public void setMessage(String message) {
        request.getSession().setAttribute("message", message);
    }

    public String getMessage() {
        return (String) request.getSession().getAttribute("message");
    }
}
