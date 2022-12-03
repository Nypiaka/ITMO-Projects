package ru.itmo.wp.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

public class StaticServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        boolean counter = true;
        for (String currentUri : request.getRequestURI().split("\\+")) {
            File file = new File("./src/main/webapp/static/" + currentUri).isFile() ?
                    new File("./src/main/webapp/static/" + currentUri) :
                    new File(getServletContext().getRealPath("/static/" + currentUri));
            if (file.isFile()) {
                if (counter) {
                    response.setContentType(getServletContext().getMimeType(file.getName()));
                    counter = false;
                }
                try (OutputStream outputStream = response.getOutputStream()) {
                    Files.copy(file.toPath(), outputStream);
                    outputStream.flush();
                }
            } else {
                response.sendError(HttpServletResponse.SC_NOT_FOUND);
            }

        }

    }
}
