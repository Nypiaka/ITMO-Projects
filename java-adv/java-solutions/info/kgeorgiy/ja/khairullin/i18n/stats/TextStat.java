package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

public class TextStat extends AbstractStat<Long> {
    private final String fileName;

    public TextStat(List<Long> data, String name, Locale locale, ResourceBundle bundle) {
        super(locale, data, bundle);
        this.fileName = name;
        super.format = String.join(System.lineSeparator(),
                bundle.getString("an_file") + " {0}",
                bundle.getString("com_stat"),
                "    " + bundle.getString("sen_num") + ": {1, number}.",
                "    " + bundle.getString("word_num") + ": {2, number}.",
                "    " + bundle.getString("num_num") + ": {3, number}.",
                "    " + bundle.getString("money_num") + ": {4, number}.",
                "    " + bundle.getString("date_num") + ": {5, number}.", ""
        );
    }

    @Override
    public String getStat() {
        return getCommonStat(new Object[]{
                this.fileName,
                data.get(0),
                data.get(1),
                data.get(2),
                data.get(3),
                data.get(4)
        });
    }
}
