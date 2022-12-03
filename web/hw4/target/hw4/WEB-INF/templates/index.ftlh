<#-- @ftlvariable name="posts" type="ru.itmo.web.hw4.model.Post[]" -->
<#-- @ftlvariable name="p" type="ru.itmo.web.hw4.model.Post" -->
<#import "./common.ftlh" as c>

<@c.page "Home">
    <#list posts?reverse as p>
        <@c.post post=p users=users/>
    </#list>
</@c.page>