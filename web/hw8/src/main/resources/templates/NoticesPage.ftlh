<#import "commons.ftlh" as c>

<@c.page>
    <div class="form-box">
        <div class="header">Notice</div>
        <div class="body">
            <form method="post" action="">
                <div class="field">
                    <div class="name">
                        <label for="content">Content</label>
                    </div>
                    <div class="value">
                        <textarea id="content" name="content">${noticeCreateForm.content!}</textarea>
                    </div>
                    <@c.error "noticeCreateForm.content"/>
                </div>
                <div class="button-field">
                    <input type="submit" value="Create">
                </div>
            </form>
        </div>
    </div>
</@c.page>
