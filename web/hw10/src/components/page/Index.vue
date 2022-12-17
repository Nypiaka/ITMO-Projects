<template>
  <div class="posts">
    <Post v-for="post in sortedPosts" :post="post" :numOfComments=numOfCommentsByPost(post) :author=postAuthor(post)
          :key="post.id"/>
  </div>
</template>


<script>
import Post from "./Post";

export default {
  name: "Index",
  props: ["posts", "comments", "users"],
  components: {
    Post
  },
  computed: {
    sortedPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id)
    }
  },
  methods: {
    numOfCommentsByPost: function (post) {
      return Object.values(this.comments).filter(comment => comment.postId === post.id).length;
    },
    postAuthor: function (post) {
      return this.users[post.userId].login;
    }
  }
}
</script>

<style scoped>

</style>
