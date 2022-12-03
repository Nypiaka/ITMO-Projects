<#-- @ftlvariable name="comment" type="ru.itmo.wp.domain.Comment" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="post by id">
        <div class="caption" style="color: #3B5998 ;font-size: large; font-weight: bold">Post</div>
        <#if !current_post?has_content>
            <tr class="noPost">
                No such post
            </tr>
        <#else>
            <@c.post post=current_post/>
            <div class="caption" style="color: #3B5998 ;font-size: large; font-weight: bold">Tags</div>
            <#list current_post.alphTags as tag>
                <div class="body"
                     style="border-left: 4px solid var(--border-color); padding-left: 0.75rem; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;">${tag}</div>
            </#list>
            <#if user??>
                <div class="form">
                    <div class="header">Write Comment</div>
                    <div class="body">
                        <form method="post" action="">
                            <div class="field">
                                <div class="name">
                                    <label for="text">Text</label>
                                </div>
                                <div class="value">
                                    <textarea id="text" name="text">${comment.text!}</textarea>
                                </div>
                                <@c.error "comment.text"/>
                            </div>
                            <div class="button-field">
                                <input type="submit" value="Write">
                            </div>
                        </form>
                    </div>
                </div>
            </#if>
            <div class="caption" style="color: #3B5998 ;font-size: large; font-weight: bold">Comments</div>
            <#list current_post.comments as comment>
                <div class="information" style=" margin-top: 0.25rem; font-size: 0.85rem; color: #888;">
                    By ${comment.user.login}, ${comment.creationTime}</div>
                <div class="body"
                     style="border-left: 4px solid var(--border-color); padding-left: 0.75rem; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;">${comment.text}</div>
            </#list>
        </#if>
    </div>
</@c.page>
