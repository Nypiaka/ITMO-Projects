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
  props: ["posts"],
  components: {
    Post
  },
  computed: {
    sortedPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id)
    }
  },
  methods: {
    // eslint-disable-next-line no-unused-vars
    numOfCommentsByPost: function (post) {
      return 0;
    },
    postAuthor: function (post) {
      return post.user.login;
    }
  }
}
</script>

<style scoped>

</style>