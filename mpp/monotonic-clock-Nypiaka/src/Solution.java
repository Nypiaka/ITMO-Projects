import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author :KHairullin Aleksandr
 */
public class Solution implements MonotonicClock {
    private final RegularInt c1 = new RegularInt(0);
    private final RegularInt c2 = new RegularInt(0);
    private final RegularInt c3 = new RegularInt(0);

    private final RegularInt c1_ = new RegularInt(0);
    private final RegularInt c2_ = new RegularInt(0);
    private final RegularInt c3_ = new RegularInt(0);

    @Override
    public void write(@NotNull Time time) {
        c1.setValue(time.getD1());
        c2.setValue(time.getD2());
        c3.setValue(time.getD3());
        c3_.setValue(c3.getValue());
        c2_.setValue(c2.getValue());
        c1_.setValue(c1.getValue());
    }

    @NotNull
    @Override
    public Time read() {
        var d1_ = c1_.getValue();
        var d2_ = c2_.getValue();
        var d3_ = c3_.getValue();
        var d3 = c3.getValue();
        var d2 = c2.getValue();
        var d1 = c1.getValue();
        if (d1 == d1_) {
            if (d2 == d2_) {
                if (d3 == d3_) {
                    return new Time(d1, d2, d3);
                }
                return new Time(d1, d2, d3);
            }
            return new Time(d1, d2, 0);
        }
        return new Time(d1, 0, 0);
    }
}
