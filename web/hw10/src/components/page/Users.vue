
<template>
  <div class="users datatable">
    <div class="caption">Users</div>
    <table>
      <thead>
      <tr>
        <th>Id</th>
        <th>Login</th>
        <th>Name</th>
      </tr>
      </thead>
      <UsersComponent v-for="user in users" :user="user" :key="user.id"/>
    </table>
  </div>
</template>

<script>
import UsersComponent from "./UsersComponent.vue";
export default {
  name: "Users",
  components: {UsersComponent},
  props: ["users"]
}
</script>

<style scoped>
</style>