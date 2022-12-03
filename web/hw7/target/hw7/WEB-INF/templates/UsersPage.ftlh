<#-- @ftlvariable name="users" type="ru.itmo.wp.model.domain.User[]" -->
<#-- @ftlvariable name="user" type="ru.itmo.wp.model.domain.User" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="users datatable">
        <div class="caption">User</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <#if user?has_content && user.admin>
                    <th>Admin</th>
                </#if>
            </tr>
            </thead>
            <tbody>
            <template>
                <tr>
                    <td class="user-id"></td>
                    <td class="user_login"></td>
                    <#if user?has_content && user.admin>
                        <td class="user-admin">
                            <button type="button"></button>
                        </td>
                    </#if>
                </tr>
            </template>
            <tr class="noData">
                <td colspan="3">
                    No data
                </td>
            </tr>
            </tbody>
        </table>
        <div class="error" style="color: red"></div>
    </div>

    <script>
        $(function () {
            const $noData = $(".users .noData");
            $noData.find("td").text("Loading...");
            const $error = $(this).find(".error");
            $.ajax({
                dataType: "json",
                data: {
                    action: "findAll"
                },
                success: function (response) {
                    response["users"].forEach(function (user) {
                        $noData.hide();
                        const $tr = $($(".users template").prop("content")).clone();
                        $tr.find(".user-id").text(user["id"]);
                        $tr.find(".user_login").text(user["login"]);
                        if (user["admin"]) {
                            $tr.find(".user-admin button").text("disable");
                        } else {
                            $tr.find(".user-admin button").text("enable");
                        }
                        $tr.find("tr").attr("data-userId", user["id"]);
                        $tr.find("button").attr("data-userId", user["id"]);
                        $(".users tbody").append($tr);
                    });
                    $(".users button").click(function () {
                        const button = $(this);
                        $.ajax({
                            dataType: "json",
                            data: {
                                action: "changeAdmin",
                                buttonText: button.text(),
                                changeAdminId: $(this).attr("data-userId")
                            },
                            success: function (response) {
                                if (response["error"]) {
                                    $error.text(response["error"]);
                                } else {
                                    if (button.text() === "enable") {
                                        button.text("disable");
                                        notify("Admin enabled");
                                    } else {
                                        button.text("enable");
                                        notify("Admin disabled");
                                    }
                                }
                            },
                            type: "POST"
                        })
                    });
                },
                type: "POST"
            });
        })
    </script>
</@c.page>
