package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.security.Guest;
import ru.itmo.wp.service.PostService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {
    private final PostService postService;

    public PostPage(PostService postService) {
        this.postService = postService;
    }

    @Guest
    @GetMapping("/post/{id}")
    public String postPage(@PathVariable String id, Model model) {
        Post resultPost = null;
        try {
            resultPost = postService.findById(Long.valueOf(id));
        } catch (NumberFormatException ignored) {
        }
        model.addAttribute("current_post", resultPost);
        model.addAttribute("comment", new Comment());
        return "PostPage";
    }

    @Guest
    @GetMapping("/post/")
    public String postPage(Model model) {
        model.addAttribute("current_post", null);
        return "PostPage";
    }


    @PostMapping("/post/{id}")
    public String writeCommentPost(@PathVariable String id,
                                   @Valid @ModelAttribute Comment comment,
                                   BindingResult bindingResult,
                                   HttpSession httpSession, Model model) {
        if (bindingResult.hasErrors()) {
//            putErrorMessage(httpSession, "Invalid comment");
            model.addAttribute("current_post", postService.findById(Long.valueOf(id)));
            return "PostPage";
        }
        postService.writeComment(postService.findById(Long.valueOf(id)), comment);
        putMessage(httpSession, "Comment created successful");
        return "redirect:/post/" + id;
    }
}
