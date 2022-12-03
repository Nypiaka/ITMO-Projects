package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.service.UserService;

@Controller
public class UserPage extends Page {
    private final UserService userService;

    public UserPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/user/{id}")
    public String userPage(@PathVariable String id, Model model) {
        User resultUser = null;
        try {
            resultUser = userService.findById(Long.valueOf(id));
        } catch (NumberFormatException ignored) {
        }
        model.addAttribute("current_user", resultUser);
        return "UserPage";
    }

    @GetMapping("/user/")
    public String userPage(Model model) {
        model.addAttribute("current_user", null);
        return "UserPage";
    }
}
