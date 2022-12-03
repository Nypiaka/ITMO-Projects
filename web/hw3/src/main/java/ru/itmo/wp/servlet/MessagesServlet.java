package ru.itmo.wp.servlet;

import com.google.gson.Gson;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;
import java.io.IOException;
import java.util.*;

public class MessagesServlet extends HttpServlet {
    private ArrayList<Map<String, String>> usersToMessages = new ArrayList<>();

    private void setJson(HttpServletResponse resp, Object jsonable) throws IOException {
        resp.getWriter().print(new Gson().toJson(jsonable));
        resp.getWriter().flush();
        resp.setContentType("application/json");
    }


    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        HttpSession currentSession = req.getSession();
        String uri = req.getRequestURI();
        if (uri.endsWith("auth")) {
            if (req.getParameter("user") != null) {
                if (currentSession.getAttribute("user") == null) {
                    currentSession.setAttribute("user", req.getParameter("user").trim());
                }

            }
            setJson(resp, currentSession.getAttribute("user"));
        } else if (uri.endsWith("add")) {
            if (req.getParameter("text").trim().length() != 0) {
                currentSession.setAttribute("text", req.getParameter("text"));
            } else {
                currentSession.setAttribute("text", null);
            }
            usersToMessages.add(Map.of("user", (String) currentSession.getAttribute("user"), "text", (String) currentSession.getAttribute("text")));
            setJson(resp, currentSession.getAttribute("text"));
        } else if (uri.endsWith("findAll")) {
            if (currentSession.getAttribute("user") != null) {
                setJson(resp, this.usersToMessages);
            }
        }
    }

}

