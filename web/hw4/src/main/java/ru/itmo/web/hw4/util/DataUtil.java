package ru.itmo.web.hw4.util;

import ru.itmo.web.hw4.model.Color;
import ru.itmo.web.hw4.model.Post;
import ru.itmo.web.hw4.model.User;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

public class DataUtil {
    private static final List<User> USERS = Arrays.asList(
            new User(1, "MikeMirzayanov", "Mike Mirzayanov", Color.GREEN),
            new User(6, "pashka", "Pavel Mavrin", Color.RED),
            new User(9, "geranazavr555", "Georgiy Nazarov", Color.BLUE),
            new User(11, "tourist", "Gennady Korotkevich", Color.RED)
    );

    private static final List<Post> POSTS = Arrays.asList(
            new Post(1, "Пластмассовый мир", "Пластмассовый мир победил\n" +
                    "Макет оказался сильней\n" +
                    "Последний кораблик остыл\n" +
                    "Последний фонарик устал,\n" +
                    "         а в горле сопят комья воспоминаний\n" +
                    "\n" +
                    "         Оо- моя оборона\n" +
                    "         Солнечный зайчик стеклянного глаза\n" +
                    "         Оо- моя оборона\n" +
                    "         Траурный мячик нелепого мира\n" +
                    "         Траурный мячик дешёвого мира\n" +
                    "\n" +
                    "Пластмассовый мир победил.\n" +
                    "Ликует картонный набат-\n" +
                    "          кому нужен ломтик июльского неба?\n" +
                    "\n" +
                    "          Оо- моя оборона\n" +
                    "          Солнечный зайчик незрячего мира\n" +
                    "          Оо- моя оборона\n" +
                    "          Траурный мячик стеклянного глаза\n" +
                    "          Траурный зайчик нелепого мира...\n" +
                    "\n" +
                    "Пластмассовый мир победил\n" +
                    "Макет оказался сильней\n" +
                    "Последний кораблик остыл\n" +
                    "Последний фонарик устал,\n" +
                    "          а в горле сопят комья воспоминаний.\n" +
                    "\n" +
                    "          Оо- моя оборона\n" +
                    "          Траурный мячик незрячего мира\n" +
                    "          Оо- моя оборона\n" +
                    "          Солнечный зайчик стеклянного глаза.\n" +
                    "\n" +
                    "          Оо- моя оборона!\n", 6),
            new Post(11, "Всё идёт по плану", "Границы ключ переломлен пополам\n" +
                    "А наш батюшка Ленин совсем усоп\n" +
                    "Он разложился на плесень и на липовый мёд\n" +
                    "А перестройка всё идёт и идёт по плану\n" +
                    "И вся грязь превратилась в голый лёд\n" +
                    "И всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "А моя судьба захотела на покой\n" +
                    "Я обещал ей не участвовать в военной игре\n" +
                    "Но на фуражке, на моей, серп и молот, и звезда\n" +
                    "Как это трогательно: серп и молот, и звезда\n" +
                    "Лихой фонарь ожидания мотается\n" +
                    "И всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "А моей женой накормили толпу\n" +
                    "Мировым кулаком растоптали ей грудь\n" +
                    "Всенародной свободой растерзали ей плоть\n" +
                    "Так закопайте ж её во Христе\n" +
                    "И всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "Один лишь дедушка Ленин хороший был вождь\n" +
                    "А все другие остальные такое дерьмо\n" +
                    "А все другие враги и такие дураки\n" +
                    "Над родною, над Отчизной бесноватый снег шёл\n" +
                    "Я купил журнал \"Корея\" — там тоже хорошо\n" +
                    "Там товарищ Ким Ир Сен, там то же, что у нас\n" +
                    "Я уверен, что у них то же самое\n" +
                    "И всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "А при коммунизме всё будет хорошо\n" +
                    "Он наступит скоро, надо только подождать\n" +
                    "Там всё будет бесплатно, там всё будет в кайф\n" +
                    "Там, наверное, вообще не надо будет умирать\n" +
                    "Я проснулся среди ночи и понял, что\n" +
                    "Всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "А всё идёт по плану\n" +
                    "Всё идёт по плану\n" +
                    "Всё", 9),
            new Post(111, "Евангелие", "Зоркие окна\n" +
                    "Кто согреет зоркие окна?\n" +
                    "Пожалей беззвучными словами\n" +
                    "Своего оловянного Xриста\n" +
                    "Беглые тени...\n" +
                    "Кто поймает беглые тени?\n" +
                    "Спеленай надёжными цепями\n" +
                    "Своего безнадёжного Xриста\n" +
                    "Жадные пальцы...\n" +
                    "Кто накормит жадные пальцы?\n" +
                    "Обними голодными руками\n" +
                    "Своего неспасённого Xриста\n" +
                    "Скользкие вены...\n" +
                    "Скользкие тревожные вены\n" +
                    "Поцелуй холодными губами\n" +
                    "Своего зазеркального Xриста\n" +
                    "Круглое небо...\n" +
                    "Кто накажет круглое небо?\n" +
                    "Задуши послушными руками\n" +
                    "Своего непослушного Xриста.", 9),
            new Post(1111, "Под небом голубым", "Под небом голубым есть город золотой,\n" +
                    "С прозрачными воротами и яркою звездой.\n" +
                    "А в городе том сад, все травы да цветы;\n" +
                    "Гуляют там животные невиданной красы.\n" +
                    "Одно - как желтый огнегривый лев,\n" +
                    "Другое - вол, исполненный очей;\n" +
                    "С ними золотой орел небесный,\n" +
                    "Чей так светел взор незабываемый.\n" +
                    "А в небе голубом горит одна звезда;\n" +
                    "Она твоя, о ангел мой, она твоя всегда.\n" +
                    "Кто любит, тот любим, кто светел, тот и свят;\n" +
                    "Пускай ведет звезда тебя дорогой в дивный сад.\n" +
                    "Тебя там встретит огнегривый лев,\n" +
                    "И синий вол, исполненный очей;\n" +
                    "С ними золотой орел небесный,\n" +
                    "Чей так светел взор незабываемый.", 1)

    );

    {
        Collections.reverse(USERS);
    }

    public static void addData(HttpServletRequest request, Map<String, Object> data) {
        data.put("users", USERS);
        data.put("posts", POSTS);
        for (User user : USERS) {
            if (Long.toString(user.getId()).equals(request.getParameter("logged_user_id"))) {
                data.put("user", user);
            }
        }
    }
}
