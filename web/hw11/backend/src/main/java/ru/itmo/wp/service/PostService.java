package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.form.PostCredentials;
import ru.itmo.wp.repository.PostRepository;

import java.util.List;

@Service
public class PostService {
    private final PostRepository postRepository;

    public PostService(PostRepository postRepository) {
        this.postRepository = postRepository;
    }

    public List<Post> findAll() {
        return postRepository.findAllByOrderByCreationTimeDesc();
    }

    public void createPost(PostCredentials postCreationForm) {
        Post post = new Post();
        post.setText(postCreationForm.getText());
        post.setTitle(postCreationForm.getTitle());
        post.setUser(postCreationForm.getUser());
        postRepository.save(post);
    }
}
