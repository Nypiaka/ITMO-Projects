<template>
  <div class="form">
    <div class="header">Write Post</div>
    <div class="body">
      <form @submit.prevent="onWritePost">
        <input type="hidden" name="action" value="writePost"/>
        <div class="field">
          <div class="name">
            <label for="title">Title</label>
          </div>
          <div class="value">
            <input autofocus id="title" name="title" v-model="title">
          </div>
        </div>
        <div class="field">
          <div class="name">
            <label for="text">Text</label>
          </div>
          <div class="value">
            <textarea id="text" name="text" v-model="text"></textarea>
          </div>
        </div>
        <div class="field error">{{ error }}</div>
        <div class="button-field">
          <input type="submit" value="Write">
        </div>
      </form>
    </div>
  </div>
</template>


<script>
export default {
  name: "WritePost",
  data: function () {
    return {
      title: "",
      text: "",
      error: ""
    }
  },
  methods: {
    onWritePost: function () {
      this.$root.$emit("onWritePost", this.title, this.text);
    }
  },
  beforeCreate() {
    this.$root.$on("onWritePostValidationError", (error) => this.error = error);
  }
}
</script>

<style scoped>

</style>