package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class MyArticlesPage {
    private void action(HttpServletRequest request, Map<String, Object> view) {
        User user = (User) request.getSession().getAttribute("user");
        try {
            userService.validateUser(user);
        } catch (ValidationException e) {
            request.getSession().setAttribute("message", "You have to be logged to see Articles");
            throw new RedirectException("/index");
        }
        view.put("articles", articleService.findAllByUser(user.getId()));

    }

    private final ArticleService articleService = new ArticleService();

    private final UserService userService = new UserService();

    private void changeHidden(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        User sessionUser = (User) request.getSession().getAttribute("user");

        String articleId = request.getParameter("articleId");

        Article article = articleService.find(Long.parseLong(articleId));

        articleService.validateArticle(article);

        String buttonText = request.getParameter("buttonText");

        if (buttonText == null) {
            request.getSession().setAttribute("message", "Button text required");
            throw new RedirectException("/index");
        }

        articleService.validateArticleAndUser(sessionUser, article);

        articleService.validateArticleAndButton(article, buttonText);

        articleService.updateHidden(article, buttonText.equals("Show"));
    }


    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("articles", articleService.findAllByUser(((User) request.getSession().getAttribute("user")).getId()));
        view.put("users", userService.findAll());
    }
}
