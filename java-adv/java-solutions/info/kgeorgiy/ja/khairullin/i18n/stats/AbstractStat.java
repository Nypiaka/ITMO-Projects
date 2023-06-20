package info.kgeorgiy.ja.khairullin.i18n.stats;

import java.text.MessageFormat;
import java.util.*;
import java.util.function.Function;
import java.util.function.ToDoubleFunction;

public abstract class AbstractStat<T> {
    protected final Locale locale;

    protected final ResourceBundle bundle;
    private static final String EMPTY = "-";

    protected final List<T> data;

    protected String format;

    public AbstractStat(Locale locale, List<T> data, ResourceBundle bundle) {
        this.data = data;
        this.locale = locale;
        this.bundle = bundle;
    }

    public abstract String getStat();

    protected String getCommonStat(Comparator<? super T> comparator, ToDoubleFunction<? super T> mapper,
                                   Function<Double, T> creator) {
        MessageFormat messageFormat = new MessageFormat(this.format, this.locale);
        T minData = data.stream().min(comparator).orElse(null);

        T maxData = data.stream().max(comparator).orElse(null);

        T avgData = creator.apply(data.stream().mapToDouble(mapper).average().orElse(0));

        return messageFormat.format(new Object[]{
                data.size(),
                new HashSet<>(data).size(),
                minData == null ? 0 : minData,
                maxData == null ? 0 : maxData,
                avgData == null ? 0 : avgData});
    }

    protected String getCommonStat(Object[] args) {
        MessageFormat messageFormat = new MessageFormat(this.format, this.locale);
        return messageFormat.format(args);
    }
}
