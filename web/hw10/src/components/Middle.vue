<template>
  <div class="middle">
    <Sidebar :posts="viewPosts"/>
    <main>
      <Index :posts="posts" :comments="comments" :users="users" v-if="page === 'Index'"/>
      <Enter v-if="page === 'Enter'"/>
      <Register v-if="page === 'Register'"/>
      <WritePost v-if="page === 'WritePost'"/>
      <EditPost v-if="page === 'EditPost'"/>
      <Users :users="users" v-if="page === 'Users'"/>
    </main>
  </div>
</template>

<script>
import Sidebar from "./sidebar/Sidebar";
import Index from "./page/Index";
import Enter from "./page/Enter";
import Register from "./page/Register";
import WritePost from "./page/WritePost";
import EditPost from "./page/EditPost";
import Users from "./page/Users.vue";

export default {
  name: "Middle",
  data: function () {
    return {
      page: "Index"
    }
  },
  components: {
    WritePost,
    Enter,
    Register,
    Index,
    Sidebar,
    Users,
    EditPost
  },
  props: ["posts", "users", "comments"],
  computed: {
    viewPosts: function () {
      return Object.values(this.posts).sort((a, b) => b.id - a.id).slice(0, 2);
    }
  }, beforeCreate() {
    this.$root.$on("onChangePage", (page) => this.page = page)
  }
}
</script>

<style scoped>

</style>
