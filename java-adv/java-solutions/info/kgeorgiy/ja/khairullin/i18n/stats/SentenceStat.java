package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

public class SentenceStat extends WordStat {
    public SentenceStat(List<String> data, Locale locale, ResourceBundle bundle) {
        super(data, locale, bundle);
        super.format = data.size() != 0 ? String.join(System.lineSeparator(), bundle.getString("sen_stat"),
                "    " + bundle.getString("sen_num") + ": {0, number} ({1, number} " + bundle.getString("var") + ").",
                "    " + bundle.getString("min_sen") + ": \"{2}\".",
                "    " + bundle.getString("max_sen") + ": \"{3}\".",
                "    " + bundle.getString("min_sen_len") + ": {4, number} (\"{5}\").",
                "    " + bundle.getString("max_sen_len") + ": {6, number} (\"{7}\").",
                "    " + bundle.getString("avg_sen_len") + ": {8, number}.", ""
        ) : String.join(System.lineSeparator(), bundle.getString("sen_stat"), "    " + bundle.getString("not_found"), "");
    }

    @Override
    public String getStat() {
        return super.getStat();
    }
}
