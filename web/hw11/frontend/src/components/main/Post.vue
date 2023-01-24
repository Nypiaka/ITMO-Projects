<template>
  <article>
    <div class="title"><a href="#" @click.prevent="">{{ post.title }}</a></div>
    <div class="information">By {{ author }}</div>
    <div class="body">{{ post.text }}</div>
    <ul class="attachment">
      <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
      <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
    </ul>
    <div class="footer">
      <div class="left">
        <img src="@/assets/img/voteup.png" title="Vote Up" alt="Vote Up"/>
        <span class="positive-score">+173</span>
        <img src="@/assets/img/votedown.png" title="Vote Down" alt="Vote Down"/>
      </div>
      <div class="right">
        <img src="@/assets/img/comments_16x16.png" title="Comments" alt="Comments"/>
        <a href="#">{{ numOfComments }}</a>
      </div>
    </div>
  </article>
</template>

<script>
export default {
  name: "Post",
  props: ["post", "comments", "users", "numOfComments", "author"],
}
</script>

<style scoped>
/* article */
article {
  margin-bottom: 2em;
}

article .title {
  color: var(--caption-color);
  font-weight: bold;
  font-size: 1.25rem;
}

article .information {
  margin-top: 0.25rem;
  font-size: 0.85rem;
  color: #888;
}

article .body {
  border-left: 4px solid var(--border-color);
  padding-left: 0.75rem;
  font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
}

article .body p:last-child {
  margin: 0;
}

article .attachment {
  padding: 0;
  margin: 0.5rem 0 0 0;
}

article .attachment li {
  list-style: none;
  padding: 0.25rem 20px;
  margin: 0;
  background: url("@/assets/img/paperclip-16x16.png") 0 2px no-repeat;
  font-size: 0.75rem;
  color: #888;
}

article .footer {
  border: 1px solid var(--border-color);
  border-radius: var(--border-radius);
  overflow: hidden;
  padding: 0.1rem;
  margin-top: 0.25rem;
}

article .footer .left {
  float: left;
  padding-left: 0.5rem;
}

article .footer .left img {
  position: relative;
  top: 5px;
}

article .footer .right img {
  position: relative;
  margin-left: 0.5rem;
  top: 2px;
}

article .footer .right {
  float: right;
  font-size: 0.85rem;
  line-height: 2rem;
  padding-right: 0.5rem;
}

article .footer .positive-score {
  color: green;
  font-weight: bold;
  line-height: 1.75rem;
}
</style>