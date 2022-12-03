<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#assign marker=0>
    <#if handle?has_content>
        <#assign index = c.findIndexBy(users, "handle", handle)!/>
    <#elseif user_id?has_content>
        <#assign index = c.findIndexBy(users, "id", user_id)!/>
    </#if>
    <div class="user-profile"
         style="border: 1px solid var(--border-color);
                border-radius: var(--border-radius);
                overflow: hidden;">
    <div class="info" style="float: left;">
    <span style="font-size: 1.5rem;
                    padding: 0 0.5rem;">
            </span>
    <#if index?has_content>
        <#assign currentUser = users[index]/>
        <#if currentUser?has_content>
            <span style="font-size: 2rem;"> <@c.userlink user=currentUser nameOnly=0/> </span>
            <#assign marker=1>
            <ul>
                <li>id: ${currentUser.id}</li>
                <li>handle: ${currentUser.handle}</li>
                <li>name: ${currentUser.name}</li>
            </ul>
            </div>
            <img src="img/cf_avatar.jpg" alt="profile photo"
                 style="float: right;">
            </div>
            <#assign prevRef = c.nextItemByStep(users,"handle",currentUser.handle,-1)!/>
            <#if prevRef?has_content>
                <a href="/user?handle=${prevRef["handle"]}" class="prev"
                   style="color: blue; font-size: 2rem; text-decoration: none"> ⟵ </a>
            <#else>
                <span style="color: lightgray; font-size: 2rem"> ⟵ </span>
            </#if>
            <#assign nextRef = c.nextItemByStep(users,"handle",currentUser.handle,1)!/>
            <#if nextRef?has_content>
                <a href="/user?handle=${nextRef["handle"]}" class="prev"
                   style="color: blue; font-size: 2rem; text-decoration: none"> ⟶ </a>
            <#else>
                <span style="color: lightgray; font-size: 2rem"> ⟶ </span>
            </#if>
        <#else>
            No such user
        </#if>
    <#else>
        No such user
    </#if>

</@c.page>
