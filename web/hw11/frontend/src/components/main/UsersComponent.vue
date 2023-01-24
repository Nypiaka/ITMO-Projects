<template>
  <tbody>
  <tr>
    <td class="id">{{ user.id }}</td>
    <td class="login">{{ user.login }}</td>
    <td class="name">{{ user.name }}</td>
  </tr>
  </tbody>
</template>

<script>
export default {
  name: "UsersComponent",
  props: ["user"]
}
</script>

<style scoped>
</style>