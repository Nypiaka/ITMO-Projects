<template>
    <footer>
        <a href="/">Codeforces</a> &copy; 2022 by Mike Mirzayanov | {{numOfPosts}} posts | {{numOfUsers}} users
    </footer>
</template>

<script>
export default {
    name: "Footer",
    props: ["numOfUsers", "numOfPosts"]
}
</script>

<style scoped>

</style>