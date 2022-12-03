<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="my-articles datatable">
        <div class="caption">Articles</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Title</th>
                <th>Show/Hide</th>
            </tr>
            </thead>
            <tbody>
            <#list articles as article>
                <tr>
                    <td class="article-id"> ${article.id} </td>
                    <td class="article-title"> ${article.title} </td>
                    <td class="article-hidden">
                        <button data-article-id=${article.id}><#if article.availability>Hide<#else>Show</#if></button>
                    </td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>

    <script>
        $(".my-articles button").click(function () {
            const button = $(this);
            const $error = $(this).find(".error");
            $.ajax({
                type: "POST",
                url: "",
                dataType: "json",
                data: {
                    action: "changeHidden",
                    articleId: $(this).attr("data-article-id"),
                    buttonText: button.text()
                },
                success: function (response) {
                    if (response["error"]) {
                        $error.text(response["error"]);
                    } else if (response["redirect"]) {
                        location.href = response["redirect"];
                    } else {
                        if (button.text() === "Hide") {
                            button.text("Show");
                            notify("Article hidden");
                        } else if (button.text() === "Show") {
                            button.text("Hide");
                            notify("Article opened");
                        }
                    }
                }
            });
        })
    </script>
</@c.page>
