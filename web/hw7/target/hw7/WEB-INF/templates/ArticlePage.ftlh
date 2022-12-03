<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="targetUserId" type="java.lang.Long" -->
<#-- @ftlvariable name="text" type="java.lang.String" -->
<#-- @ftlvariable name="error" type="java.lang.String" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="article form-box">
        <div class="header">Create Article</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="article"/>
                <div class="field">
                    <div class="name">
                        <label for="title">Title</label>
                    </div>
                    <div class="value">
                        <input id="title" name="title"/>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text"></textarea>
                    </div>
                </div>
                <div class="error"></div>
                <div class="button-field">
                    <input type="submit" value="Create">
                </div>
            </form>
        </div>
    </div>
    <script>
        $(function () {
            $(".article form").submit(function () {
                const text = $(this).find("textarea[name='text']").val();
                const title = $(this).find("input[name='title']").val();
                const $error = $(this).find(".error");

                $.ajax({
                    type: "POST",
                    url: "",
                    dataType: "json",
                    data: {
                        action: "createArticle",
                        text: text,
                        title: title

                    },
                    success: function (response) {
                        if (response["error"]) {
                            $error.text(response["error"]);
                        } else {
                            location.href = response["redirect"];
                        }
                    }
                });

                return false;
            });
        })
    </script>
</@c.page>
