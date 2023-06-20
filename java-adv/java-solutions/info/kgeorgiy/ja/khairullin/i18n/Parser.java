package info.kgeorgiy.ja.khairullin.i18n;

import info.kgeorgiy.ja.khairullin.i18n.stats.*;
import info.kgeorgiy.ja.khairullin.i18n.stats.bundles.BundleEn;
import info.kgeorgiy.ja.khairullin.i18n.stats.bundles.BundleRus;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.BreakIterator;
import java.text.DateFormat;
import java.text.NumberFormat;
import java.text.ParsePosition;
import java.util.*;
import java.util.function.Supplier;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

public class Parser {

    private final String[] args;

    public Parser(String[] args) {
        this.args = args;
    }

    private ResourceBundle parseBundleByLocale(Locale locale) {
        if (locale.getLanguage().equals("ru")) return new BundleRus();
        if (locale.getLanguage().equals("en")) return new BundleEn();
        return null;
    }

    public void parse() {
        try {
            List<Double> money = new ArrayList<>();
            List<Double> nums = new ArrayList<>();
            List<String> words = new ArrayList<>();
            List<Date> dates = new ArrayList<>();
            List<String> sentences = new ArrayList<>();
            var firstParsedLocale = args[0].split("_");
            var secondParsedLocale = args[1].split("_");
            Path intputPath = Path.of(args[2]);
            List<String> lines = Files.readAllLines(intputPath.toAbsolutePath());
            Locale textLocale = Locale.of(firstParsedLocale[0], firstParsedLocale[1]);
            Locale outputLocale = Locale.of(secondParsedLocale[0], secondParsedLocale[1]);
            String text = String.join(" ", lines);
            parseWords(dates, money, nums, words, text, textLocale);
            parseSentences(sentences, textLocale, text);
            ResourceBundle bundle = parseBundleByLocale(outputLocale);
            String parsedText = (new TextStat(new ArrayList<>(List.of(new Long[]{
                    (long) sentences.size(),
                    (long) words.size(),
                    (long) nums.size(),
                    (long) money.size(),
                    (long) dates.size()
            })), intputPath.getFileName().toString(), outputLocale, bundle).getStat()
            );
            String parsedSentences = new SentenceStat(sentences, outputLocale, bundle).getStat();
            String parsedWords = new WordStat(words, outputLocale, bundle).getStat();
            String parsedNums = new NumStat(nums, outputLocale, bundle).getStat();
            String parsedMoney = new MoneyStat(money, outputLocale, bundle).getStat();
            String parsedDates = new DateStat(dates, outputLocale, bundle).getStat();
            writeResult(parsedSentences, parsedWords, parsedNums, parsedMoney, parsedDates, parsedText, args[3]);
        } catch (IOException e) {
            throw new IllegalArgumentException("Problems with input file.");
        }
    }

    private void writeResult(String parsedSentences,
                             String parsedWords,
                             String parsedNums,
                             String parsedMoney,
                             String parsedDates,
                             String parsedText,
                             String output) {
        try (BufferedWriter writer = Files.newBufferedWriter(Path.of(output).toAbsolutePath())) {
            writer.write(parsedText);
            writer.write(parsedSentences);
            writer.write(parsedWords);
            writer.write(parsedNums);
            writer.write(parsedMoney);
            writer.write(parsedDates);
        } catch (IOException e) {
            throw new IllegalArgumentException("Problems with output file.");
        }
    }

    private void parseSentences(List<String> sentences, Locale locale, String text) {
        BreakIterator breakIterator = BreakIterator.getSentenceInstance(locale);
        breakIterator.setText(text);
        while (breakIterator.current() != text.length()) {
            sentences.add(text.substring(breakIterator.current(), breakIterator.next()));
        }
    }

    private void parseWords(List<Date> dates,
                            List<Double> money,
                            List<Double> nums,
                            List<String> words,
                            String text,
                            Locale currentLocale
    ) {
        BreakIterator breakIterator = BreakIterator.getWordInstance(currentLocale);
        breakIterator.setText(text);
        do {
            parseSomethingWithNums(dates, money, nums, breakIterator, text, currentLocale);
            if (breakIterator.current() == text.length()) break;
            String word = text.substring(breakIterator.current(), breakIterator.next());
            if (!word.isBlank() && !Pattern.matches("\\p{Punct}", word)) words.add(word);
        } while (breakIterator.current() != text.length());
    }

    private void parseSomethingWithNums(List<Date> dates,
                                        List<Double> money,
                                        List<Double> nums,
                                        BreakIterator breakIterator,
                                        String text,
                                        Locale currentLocale) {
        int prevDates;
        int prevMoney;
        int prevNums;
        do {
            prevDates = dates.size();
            prevMoney = money.size();
            prevNums = nums.size();
            tryParseDate(breakIterator, dates, text, currentLocale);
            tryParseMoney(breakIterator, money, text, currentLocale);
            tryParseNumber(breakIterator, nums, text, currentLocale);
        } while (somethingParsedSuccess(prevDates, dates.size(), prevMoney, money.size(), prevNums, nums.size()));
    }

    private boolean somethingParsedSuccess(int prevDates, int datesSize, int prevMoney, int moneySize, int prevNums, int numsSize) {
        return (prevDates != datesSize || prevMoney != moneySize || prevNums != numsSize);
    }

    private void moveBorder(BreakIterator breakIterator, int bound) {
        while (breakIterator.current() < bound) {
            breakIterator.next();
        }
    }

    private void tryParseNumber(BreakIterator breakIterator, List<Double> nums, String text, Locale textLocale) {
        ParsePosition runner = new ParsePosition(breakIterator.current());
        tryParseCommon(breakIterator, nums, () -> {
            try {
                return NumberFormat.getNumberInstance(textLocale).parse(text, runner).doubleValue();
            } catch (NullPointerException e) {
                return null;
            }
        }, runner);
    }

    private void tryParseMoney(BreakIterator breakIterator, List<Double> money, String text, Locale textLocale) {
        ParsePosition runner = new ParsePosition(breakIterator.current());
        tryParseCommon(breakIterator, money, () -> {
            try {
                return NumberFormat.getCurrencyInstance(textLocale).parse(text, runner).doubleValue();
            } catch (NullPointerException e) {
                return null;
            }
        }, runner);
    }

    private <T> void tryParseCommon(BreakIterator breakIterator, List<T> data, Supplier<T> parser, ParsePosition runner) {
        T result = parser.get();
        if (breakIterator.current() == runner.getIndex()) return;
        moveBorder(breakIterator, runner.getIndex());
        data.add(result);
    }

    private void tryParseDate(BreakIterator breakIterator, List<Date> dates, String text, Locale textLocale) {
        IntStream.of(DateFormat.SHORT, DateFormat.DEFAULT, DateFormat.FULL, DateFormat.LONG, DateFormat.MEDIUM).forEach(token -> {
            ParsePosition runner = new ParsePosition(breakIterator.current());
            tryParseCommon(breakIterator, dates, () -> {
                try {
                    return DateFormat.getDateInstance(token, textLocale).parse(text, runner);
                } catch (NullPointerException e) {
                    return null;
                }
            }, runner);
        });
    }

}
