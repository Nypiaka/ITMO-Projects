package expression;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;

public class Const implements Arithmetical {
    int a;
    BigDecimal b1;
    int cntr;

    @Override
    public int returnPriority() {
        return 9;
    }

    public Const(int b) {
        a = b;
        b1 = BigDecimal.valueOf(b);
        cntr = 1;
    }

    public Const(BigDecimal b) {
        b1 = b;
        a = b.setScale(0, RoundingMode.HALF_UP).intValueExact();
        cntr = 2;
    }

    @Override
    public int evaluate(int c) {
        return a;
    }

    @Override
    public BigDecimal evaluate(BigDecimal c) {
        return b1;
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return a;
    }

    @Override
    public String toString() {
        if (cntr == 1) return Integer.toString(a);
        else return b1.toString();
    }

    @Override
    public String toMiniString() {
        if (cntr == 1) return Integer.toString(a);
        else return b1.toString();
    }

    @Override
    public int hashCode() {
        if (cntr == 1) {
            return a;
        } else {
            return b1.hashCode();
        }
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass()) {
            if (cntr == 1) {
                return this.a == ((Const) object).a;
            } else {
                return b1.compareTo(((Const) object).b1) == 0;
            }
        }
        return false;

    }

}
