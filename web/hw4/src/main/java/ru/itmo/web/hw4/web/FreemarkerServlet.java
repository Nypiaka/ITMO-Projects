package ru.itmo.web.hw4.web;

import freemarker.template.*;
import ru.itmo.web.hw4.util.DataUtil;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class FreemarkerServlet extends HttpServlet {
    private static final String UTF_8 = StandardCharsets.UTF_8.name();
    private static final String DEBUG_TEMPLATES_PATH = "../../src/main/webapp/WEB-INF/templates";
    private static final String TEMPLATES_PATH = "/WEB-INF/templates";

    private final Configuration freemarkerConfiguration = new Configuration(Configuration.VERSION_2_3_31);

    @Override
    public void init() throws ServletException {
        File dir = new File(getServletContext().getRealPath("."), DEBUG_TEMPLATES_PATH);
        if (!dir.exists() || !dir.isDirectory()) {
            dir = new File(getServletContext().getRealPath(TEMPLATES_PATH));
        }

        try {
            freemarkerConfiguration.setDirectoryForTemplateLoading(dir);
        } catch (IOException e) {
            throw new ServletException("Unable to set template directory [dir=" + dir + "].", e);
        }

        freemarkerConfiguration.setDefaultEncoding(UTF_8);
        freemarkerConfiguration.setTemplateExceptionHandler(TemplateExceptionHandler.HTML_DEBUG_HANDLER);
        freemarkerConfiguration.setLogTemplateExceptions(false);
        freemarkerConfiguration.setWrapUncheckedExceptions(true);
        freemarkerConfiguration.setFallbackOnNullLoopVariable(false);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        request.setCharacterEncoding(UTF_8);
        response.setCharacterEncoding(UTF_8);

        Template template;
        try {
            String name = URLDecoder.decode(request.getRequestURI(), UTF_8) + ".ftlh";
            template = freemarkerConfiguration.getTemplate(name);
        } catch (TemplateNotFoundException ignored) {
            String templateParamethr = URLDecoder.decode(request.getRequestURI(), UTF_8);
            if (templateParamethr.equals("/")) {
                response.setStatus(HttpServletResponse.SC_TEMPORARY_REDIRECT);
                response.setHeader("Location", URLDecoder.decode(request.getContextPath() + "/index", UTF_8));
                return;
            } else {
                template = freemarkerConfiguration.getTemplate("404.ftlh");
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            }
        }

        Map<String, Object> data = getData(request);

        response.setContentType("text/html");
        try {
            template.process(data, response.getWriter());
        } catch (TemplateException e) {
            e.printStackTrace();
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        }
    }

    private Map<String, Object> getData(HttpServletRequest request) {
        Map<String, Object> data = new HashMap<>();

        for (Map.Entry<String, String[]> e : request.getParameterMap().entrySet()) {

            if (e.getValue() != null && e.getValue().length == 1) {
                if (e.getKey().endsWith("user_id")) {
                    try {
                        data.put(e.getKey(), Long.valueOf(e.getValue()[0]));
                    } catch (Exception numberFormatExceptionError) {
                        System.out.println("Wrong Id");
                        data.put(e.getKey(), null);
                    }
                } else {
                    data.put(e.getKey(), e.getValue()[0]);
                }
            }
        }

        DataUtil.addData(request, data);
        return data;
    }
}
