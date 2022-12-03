<#import "commons.ftlh" as c>
<#import "/spring.ftl" as spring>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <th>Condition</th>
            </tr>
            </thead>
            <tbody>
            <#if !users?has_content>
                <tr class="noData">
                    <td colspan="4">
                        No data
                    </td>
                </tr>
            <#else>
                <#list users as user>
                    <tr>
                        <td class="id">${user.id}</td>
                        <td class="login">${user.login}</td>
                        <td class="creationTime">${user.creationTime}</td>
                        <td class="disabled">
                            <form method="post" action="">
                                <input type="hidden" name="userId" value="${user.id?c}">
                                <div class="button-field">
                                    <#if user.status>
                                        <input type="submit" name="status" value="Disable">
                                    <#else >
                                        <input type="submit" name="status" value="Enable">
                                    </#if>
                                </div>
                            </form>

                        </td>
                    </tr>
                </#list>
            </#if>
            </tbody>
        </table>
        <@c.error "userCondition.status"/>
    </div>
</@c.page>
