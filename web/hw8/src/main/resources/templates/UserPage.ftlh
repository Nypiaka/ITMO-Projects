<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <#if !current_user?has_content>
        <tr class="noUser">
            No such user
        </tr>
        <#else>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
            </tr>
            </thead>
            <tbody>
            <td class="id">${current_user.id}</td>
            <td class="login">${current_user.login}</td>
            <td class="creationTime">${current_user.creationTime}</td>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>
