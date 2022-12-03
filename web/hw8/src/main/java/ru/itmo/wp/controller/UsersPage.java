package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.form.UserConditionSetCredentials;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class UsersPage extends Page {
    private final UserService userService;

    public UsersPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/users/all")
    public String users(Model model) {
        model.addAttribute("users", userService.findAll());
        model.addAttribute("userCondition", new UserConditionSetCredentials());
        return "UsersPage";
    }

    @PostMapping("/users/all")
    public String users(@Valid @ModelAttribute("userCondition") UserConditionSetCredentials userCondition, BindingResult bindingResult, HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "redirect:/users/all";
        }
        changeCondition(userService.findById(userCondition.getUserId()), userCondition.getStatus());
        setMessage(httpSession, "User was " + (userCondition.getStatus().equals("Enable") ? "enabled" : "disabled"));
        return "redirect:/users/all";
    }
}
