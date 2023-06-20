package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.util.*;

public class MoneyStat extends AbstractStat<Double> {
    public MoneyStat(List<Double> data, Locale locale, ResourceBundle bundle) {
        super(locale, data, bundle);
        super.format = data.size() != 0 ? String.join(System.lineSeparator(),
                bundle.getString("money_stat"),
                "    " + bundle.getString("money_num") + ": {0, number} ({1, number} " + bundle.getString("var") + ").",
                "    " + bundle.getString("min_money") + ": {2, number}.",
                "    " + bundle.getString("max_money") + ": {3, number}.",
                "    " + bundle.getString("avg_money") + ": {4, number}.", ""
        ) : String.join(System.lineSeparator(), bundle.getString("money_stat"), "    " + bundle.getString("not_found"), "");
    }

    @Override
    public String getStat() {
        return getCommonStat(
                Comparator.naturalOrder(),
                Double::doubleValue,
                Double::doubleValue
        );
    }
}
