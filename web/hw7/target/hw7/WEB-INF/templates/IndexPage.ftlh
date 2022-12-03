<#import "commons.ftlh" as c>

<@c.page>

    <ul class="articles">
        <li class="no-data"></li>
        <template>
            <li>
                <article>
                    <div class="title"></div>
                    <div class="information">By <span class="author"></span>, <span class="creation-time"></span></div>
                    <div class="body"></div>
                </article>
            </li>
        </template>
    </ul>

    Welcome on Codeforces!
    <script>
        $(function () {
            const $articles = $(".articles");
            $articles.find(".no-data").text("Loading articles...");
            $.ajax({
                dataType: "json",
                data: {
                    action: "findAllAvailable"
                },
                success: function (response) {
                    $articles.find(".no-data").remove()
                    response.articles.forEach(function (article) {
                        const $tpl = $($articles.find("template").prop("content")).clone();
                        $tpl.find(".title").text(article.title);
                        $tpl.find(".body").text(article.text);
                        $tpl.find(".author").text(article.userLogin);
                        $tpl.find(".creation-time").text(article.creationTime.toString());
                        $articles.append($tpl);
                    });
                },
                type: "POST"
            });
        })
    </script>
</@c.page>
