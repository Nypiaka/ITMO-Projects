package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.text.Collator;
import java.util.*;

public class WordStat extends AbstractStat<String> {
    public WordStat(List<String> data, Locale locale, ResourceBundle bundle) {
        super(locale, data, bundle);
        super.format = data.size() != 0 ? String.join(System.lineSeparator(), bundle.getString("word_stat"),
                "    " + bundle.getString("word_num") + ": {0, number} ({1, number} " + bundle.getString("var") + ").",
                "    " + bundle.getString("min_word") + ": \"{2}\".",
                "    " + bundle.getString("max_word") + ": \"{3}\".",
                "    " + bundle.getString("min_word_len") + ": {4, number} (\"{5}\").",
                "    " + bundle.getString("max_word_len") + ": {6, number} (\"{7}\").",
                "    " + bundle.getString("avg_word_len") + ": \"{8, number}\".", ""
        ) : String.join(System.lineSeparator(), bundle.getString("word_stat"), "    " + bundle.getString("not_found"), "");
    }

    @Override
    public String getStat() {
        return super.getCommonStat(
                new Object[]{
                        data.size(),
                        new HashSet<>(data).size(),
                        data.stream().min(Collator.getInstance(super.locale)).orElse("-"),
                        data.stream().max(Collator.getInstance(super.locale)).orElse("-"),
                        data.stream().mapToLong(String::length).min().orElse(0),
                        data.stream().min(Comparator.comparingInt(String::length)).orElse("-"),
                        data.stream().mapToLong(String::length).max().orElse(0),
                        data.stream().max(Comparator.comparingInt(String::length)).orElse("-"),
                        data.stream().mapToLong(String::length).average().orElse(0),
                }
        );
    }
}
