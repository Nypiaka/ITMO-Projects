<#-- @ftlvariable name="post" type="ru.itmo.web.hw4.model.Post" -->
<#macro page underHeader="">
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header underHeader/>
    <div class="middle">
        <@sidebar/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header underHeader="">
    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user=user nameOnly=1/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/users","name":"Users"},
            {"href": "/contests", "name": "Contests"}
            ]/>
            <ul>
                <#list items as item>
                    <li>
                        <#if item.name == underHeader>
                            <u style="text-decoration-color: #3B5998; text-decoration-thickness: 0.2rem"><a
                                        href="${item.href}">${item.name}</a></u>
                        <#else >
                            <a href="${item.href}">${item.name}</a>
                        </#if>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro sidebar>
    <aside>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
        <section>
            <div class="header">Information</div>
            <div class="body">Lorem ipsum dolor sit amet, consectetur adipisicing elit. Asperiores enim expedita facilis
                fugiat ipsum iste nobis reprehenderit tempore ut voluptatibus?
            </div>
            <div class="footer">
                <a href="#">View all</a>
            </div>
        </section>
    </aside>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user nameOnly>
<#-- @ftlvariable name="user" type="ru.itmo.web.hw4.model.User" -->
    <#if nameOnly?? && nameOnly == 0>
        <#if user.color == "RED">
            <a href="/user?handle=${user.handle}"
               style="color: red; text-decoration: none; font-weight: bold ">${user.name}</a>
        <#elseif user.color == "GREEN">
            <a href="/user?handle=${user.handle}"
               style="color: green; text-decoration: none; font-weight: bold">${user.name}</a>
        <#elseif user.color == "BLUE">
            <a href="/user?handle=${user.handle}"
               style="color: blue; text-decoration: none; font-weight: bold">${user.name}</a>
        </#if>
    <#else >
        <a href="/user?handle=${user.handle}">${user.name}</a>
    </#if>
</#macro>

<#macro post post users>
<#-- @ftlvariable name="post" type="ru.itmo.web.hw4.model.Post" -->
    <article>
        <#assign user=findBy(users, "id", post.user_id)>
        <#assign name=user.name>
        <div class="title">${post.title}</div>
        <div class="information">By <@userlink user=user nameOnly=0></@userlink>, 2 days ago, translation</div>
        <div class="body">
            ${post.text?truncate(350,"...")}
        </div>
        <ul class="attachment">
            <li>Announcement of <a href="#">Codeforces Round #510 (Div. 1)</a></li>
            <li>Announcement of <a href="#">Codeforces Round #510 (Div. 2)</a></li>
        </ul>
        <div class="footer">
            <div class="left">
                <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                2 days ago
                <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
    </article>

</#macro>

<#macro table users>
    <div class="datatable">
        <div class="caption">USERS</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>User</th>
                <th>Handle</th>
            </tr>
            </thead>
            <tbody>
            <#list users as u>
                <tr>
                    <td>${u.id}</td>
                    <td>
                        <@userlink user=u nameOnly=0></@userlink>
                    </td>
                    <td>${u.handle}</td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>
</#macro>

<#function findIndexBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item_index/>
        </#if>
    </#list>
</#function>


<#function findBy items key value>
    <#assign index = findIndexBy(items, key, value)>
    <#if index?has_content>
        <#return items[index]/>
    </#if>
</#function>

<#function nextItemByStep items key value step>
    <#list items as item>
        <#if item[key] == value>
            <#return items[item_index+step]/>
        </#if>
    </#list>
</#function>
