package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.util.*;

public class DateStat extends AbstractStat<Date> {
    public DateStat(List<Date> data, Locale locale, ResourceBundle bundle) {
        super(locale, data, bundle);
        super.format = data.size() != 0 ?
                String.join(System.lineSeparator(),
                        bundle.getString("date_stat"),
                        "    " + bundle.getString("date_num") + ": {0, number} ({1, number} " + bundle.getString("var") + ").",
                        "    " + bundle.getString("min_date") + ": {2, date}.",
                        "    " + bundle.getString("max_date") + ": {3, date}.",
                        "    " + bundle.getString("avg_date") + ": {4, date}.", ""
                ) : String.join(System.lineSeparator(), bundle.getString("date_stat"), "    " + bundle.getString("not_found"), "");
    }

    @Override
    public String getStat() {
        return super.getCommonStat(
                Comparator.naturalOrder(),
                Date::getTime,
                dateDouble -> new Date(dateDouble.longValue())
        );
    }
}
