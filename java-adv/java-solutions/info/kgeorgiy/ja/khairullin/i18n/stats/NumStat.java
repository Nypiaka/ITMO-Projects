package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.ResourceBundle;

public class NumStat extends AbstractStat<Double> {
    public NumStat(List<Double> data, Locale locale, ResourceBundle bundle) {
        super(locale, data, bundle);
        super.format = data.size() != 0 ? String.join(System.lineSeparator(),
                bundle.getString("num_stat"),
                "    " + bundle.getString("num_num") + ": {0, number} ({1, number} " + bundle.getString("var") + ").",
                "    " + bundle.getString("min_num") + ": {2, number}.",
                "    " + bundle.getString("max_num") + ": {3, number}.",
                "    " + bundle.getString("avg_num") + ": {4, number}.", ""
        ) : String.join(System.lineSeparator(), bundle.getString("num_stat"), "    " + bundle.getString("not_found"), "");
    }

    @Override
    public String getStat() {
        return super.getCommonStat(
                Comparator.naturalOrder(),
                Double::doubleValue,
                Double::doubleValue
        );
    }
}
