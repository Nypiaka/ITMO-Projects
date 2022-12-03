<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="talks" type="ru.itmo.wp.model.domain.Talk[]" -->
<#-- @ftlvariable name="targetUserId" type="java.lang.Long" -->
<#-- @ftlvariable name="text" type="java.lang.String" -->
<#-- @ftlvariable name="error" type="java.lang.String" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="form-box">
        <div class="header">Talk</div>
        <div class="body">
            <form method="post" action="">
                <input type="hidden" name="action" value="sendTalk"/>
                <div class="field">
                    <div class="name">
                        <label for="targetUserLogin">Choose target login</label>
                    </div>
                    <div class="value">
                        <select id="targetUserLogin" name="targetUserLogin">
                            <#list users as user>
                                <option>${user.login}</option>
                            </#list>
                        </select>
                    </div>
                </div>
                <div class="field">
                    <div class="name">
                        <label for="text">Text</label>
                    </div>
                    <div class="value">
                        <textarea id="text" name="text" type="text">${text!}</textarea>
                    </div>
                </div>
                <#if error??>
                    <div class="error">
                        ${error!}
                    </div>
                </#if>
                <div class="button-field">
                    <input type="submit" value="Send">
                </div>
            </form>
        </div>
    </div>

    <div class="datatable" style="margin-top: 5rem">
        <div class="caption">Talk</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Source User Login</th>
                <th>Target User Login</th>
                <th>Text</th>
                <th>Created</th>
            </tr>
            </thead>
            <tbody>
            <#list talks as talk>
                <tr>
                    <td>${talk.id}</td>
                    <td>${talk.sourceUser.login}</td>
                    <td>${talk.targetUser.login}</td>
                    <td>${talk.text}</td>
                    <td>${talk.creationTime}</td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
</@c.page>
