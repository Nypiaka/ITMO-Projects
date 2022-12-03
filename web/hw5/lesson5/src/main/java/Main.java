import freemarker.template.*;
import ru.itmo.wp.web.exception.NotFoundException;
import ru.itmo.wp.web.exception.RedirectException;
import ru.itmo.wp.web.page.IndexPage;
import ru.itmo.wp.web.page.NotFoundPage;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


class Meow {
    public static String print(String a) {
        System.out.println("");
        return "";
    }
}

public class Main {

    public static void main(String[] args) throws NoSuchMethodException {
        Meow a = new Meow();
        Class<? extends Meow> b = a.getClass();
        Class<?>[] m = b.getDeclaredMethod("meow").getParameterTypes();
        System.out.println(Arrays.toString(m));
    }

}
