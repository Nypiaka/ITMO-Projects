package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.form.NoticeCredentials;
import ru.itmo.wp.form.validator.NoticeCredentialsCreationValidator;
import ru.itmo.wp.service.NoticeService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class NoticesPage extends Page {
    private final NoticeService noticeService;
    private final NoticeCredentialsCreationValidator noticeCredentialsCreationValidator;

    public NoticesPage(NoticeService noticeService, NoticeCredentialsCreationValidator noticeCredentialsCreationValidator) {
        this.noticeService = noticeService;
        this.noticeCredentialsCreationValidator = noticeCredentialsCreationValidator;
    }


    @GetMapping("/notices")
    public String create(Model model) {
        model.addAttribute("noticeCreateForm", new NoticeCredentials());
        return "NoticesPage";
    }

    @PostMapping("/notices")
    public String create(@Valid @ModelAttribute("noticeCreateForm") NoticeCredentials noticeCreateForm,
                         BindingResult bindingResult,
                         HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return "NoticesPage";
        }
        noticeService.register(noticeCreateForm);
        setMessage(httpSession, "Notice created successful");
        return "redirect:";
    }
}
