package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.repository.PostRepository;
import ru.itmo.wp.repository.TagRepository;

import java.util.HashSet;
import java.util.List;

@Service
public class PostService {
    private final PostRepository postRepository;
    private final TagRepository tagRepository;

    public PostService(PostRepository postRepository, TagRepository tagRepository) {
        this.postRepository = postRepository;
        this.tagRepository = tagRepository;
    }

    public List<Post> findAll() {
        return postRepository.findAllByOrderByCreationTimeDesc();
    }

    public Post findById(Long id) {
        return id == null ? null : postRepository.findById(id).orElse(null);
    }

    public void writeComment(Post post, Comment comment) {
        post.getComments().add(comment);
        comment.setPost(post);
        comment.setUser(post.getUser());
        postRepository.save(post);
    }

    public Post toPost(PostForm postForm) {
        Post resultPost = new Post();
        String[] postTags = postForm.getTags().trim().split(" ");
        if (postTags.length != 0 && !postTags[0].equals("")) {
            resultPost.setTags(new HashSet<>());
            for (String tagName : postTags) {
                Tag foundTag = tagRepository.findTagByName(tagName);
                if (foundTag == null) {
                    foundTag = new Tag(tagName);
                    tagRepository.save(foundTag);
                }
                resultPost.getTags().add(foundTag);
            }
        }
        resultPost.setTitle(postForm.getTitle());
        resultPost.setText(postForm.getText());
        return resultPost;
    }
}
