package info.kgeorgiy.ja.khairullin.i18n;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.rmi.RemoteException;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Tests {
    private static final String pathPart = "java-advanced/java-solutions/info/kgeorgiy/ja/khairullin/i18n/TestFiles/".replace("/", File.separator);

    @Test
    public void test01_onlyNumsRu() throws IOException {
        testUniversal("ru_RU_only_nums.txt", "t01.txt", "at01.txt", "ru_RU", "en_US");
    }

    @Test
    public void test02_onlyDatesRu() throws IOException {
        testUniversal("ru_RU_only_dates.txt", "t02.txt", "at02.txt", "ru_RU", "en_US");
    }

    @Test
    public void test03_onlyMoneyRu() throws IOException {
        testUniversal("ru_RU_only_money.txt", "t03.txt", "at03.txt", "ru_RU", "en_US");
    }

    @Test
    public void test04_onlyTextRu() throws IOException {
        testUniversal("ru_RU_only_text.txt", "t04.txt", "at04.txt", "ru_RU", "en_US");
    }

    @Test
    public void test05_hodgePorridgeRu() throws IOException {
        testUniversal("ru_RU_hodgePodge.txt", "t05.txt", "at05.txt", "ru_RU", "en_US");
    }

    @Test
    public void test06_hodgePorridgeEn() throws IOException {
        testUniversal("en_US_hodgePodge.txt", "t06.txt", "at06.txt", "en_US", "ru_RU");
    }

    @Test
    public void test07_hodgePorridgeJp() throws IOException {
        testUniversal("ja_JP_hodgePodge.txt", "t07.txt", "at07.txt", "ja_JP", "ru_RU");
    }

    private void testUniversal(String aim, String ans, String trueAns, String inputLocale, String outputLocale) throws IOException {
        Path aimPath = Path.of(pathPart + aim);
        Path ansPath = Path.of(pathPart + ans);
        TextStatistics.main(new String[]{inputLocale,
                outputLocale,
                aimPath.toAbsolutePath().toString(),
                ansPath.toAbsolutePath().toString()});
        Assert.assertEquals(-1, Files.mismatch(Path.of(pathPart + trueAns), ansPath));
    }

}
