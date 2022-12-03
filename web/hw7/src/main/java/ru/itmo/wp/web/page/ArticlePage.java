package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;


/**
 * @noinspection unused
 */
public class ArticlePage {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        try {
            userService.validateUser((User) request.getSession().getAttribute("user"));
        } catch (ValidationException e) {
            request.getSession().setAttribute("message", "You have to be logged to access Article");
            throw new RedirectException("/index");
        }
    }

    private final ArticleService articleService = new ArticleService();

    private final UserService userService = new UserService();

    private void createArticle(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        if (request.getSession().getAttribute("user") == null) {
            throw new RedirectException("/index");
        }

        Article currentArticle = new Article(((User) request.getSession().getAttribute("user")).getId(),
                request.getParameter("title"), request.getParameter("text"));

        articleService.validateArticle(currentArticle);

        articleService.save(currentArticle);

        request.getSession().setAttribute("message", "Article created successful");

        throw new RedirectException("/index");
    }
}
