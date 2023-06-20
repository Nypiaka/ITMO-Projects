package info.kgeorgiy.ja.khairullin.i18n;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Locale;

public class TextStatistics {

    private static final List<Locale> LOCALES = Arrays.asList(Locale.getAvailableLocales());

    private static final String ERROR_MESSAGE = "Wrong input format. " +
            "Should be: existing locale (lang_country*), " +
            "target locale (en_country* or ru_country*), " +
            "path to existing file (string), " +
            "path to existing file (string)";

    private static boolean notValidArgs(String[] args) {
        boolean firstCheck = args == null ||
                args.length < 4 ||
                !Files.exists(Path.of(args[2]).toAbsolutePath()) ||
                !Files.exists(Path.of(args[3]).toAbsolutePath());
        if (firstCheck) {
            return true;
        }
        var firstParsedLocale = args[0].split("_");
        var secondParsedLocale = args[1].split("_");
        return firstParsedLocale.length < 2 ||
                secondParsedLocale.length < 2 ||
                !LOCALES.contains(Locale.of(firstParsedLocale[0], firstParsedLocale[1])) ||
                !LOCALES.contains(Locale.of(secondParsedLocale[0], secondParsedLocale[1])) ||
                !(secondParsedLocale[0].equals("ru") || secondParsedLocale[0].equals("en"));
    }

    public static void main(String[] args) {
        if (notValidArgs(args)) throw new IllegalArgumentException(ERROR_MESSAGE);
        Parser parser = new Parser(args);
        parser.parse();
    }

}
