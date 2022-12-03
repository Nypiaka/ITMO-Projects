package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.TalkService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class TalksPage extends Page {
    TalkService talkService = new TalkService();
    UserService userService = new UserService();

    @Override
    protected void before(HttpServletRequest request, Map<String, Object> view) {

        super.before(request, view);

        if (request.getSession().getAttribute("user") == null) {
            setMessage("You must be logged in to access the talks");
            throw new RedirectException("/index");
        }

        view.put("users", userService.findAll());

        view.put("talks", talkService.findAll(((User) request.getSession().getAttribute("user")).getId()));
    }

    protected void sendTalk(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        view.put("users", userService.findAll());

        view.put("talks", talkService.findAll(((User) request.getSession().getAttribute("user")).getId()));
        talkService.validateUser(userService.findBy("login", request.getParameter("targetUserLogin")),
                "target");

        Talk currentTalk = new Talk(((User) request.getSession().getAttribute("user")).getId(),
                (userService.findBy("login", request.getParameter("targetUserLogin")).getId()),
                request.getParameter("text"));

        talkService.validateText(request.getParameter("text"));

        currentTalk.setSourceUser(userService.findBy("login",
                ((User) request.getSession().getAttribute("user")).getLogin()));

        currentTalk.setTargetUser(userService.findBy("login", request.getParameter("targetUserLogin")));

        talkService.save(currentTalk);

        throw new RedirectException("/talks");
    }
}
