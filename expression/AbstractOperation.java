package expression;

import java.math.BigDecimal;
import java.util.Objects;

public abstract class AbstractOperation implements Arithmetical {
    Arithmetical left;
    Arithmetical right;

    @Override
    public int hashCode() {
        return Objects.hash(left, right, this.getClass());
    }

    public int eval(int left, int right) {
        return 0;
    }

    public BigDecimal eval(BigDecimal left, BigDecimal right) {
        return BigDecimal.ZERO;
    }

    public int evaluate(int a) {
        return eval(left.evaluate(a), right.evaluate(a));
    }

    public int evaluate(int a, int b, int c) {
        return eval(left.evaluate(a, b, c), right.evaluate(a, b, c));
    }

    public BigDecimal evaluate(BigDecimal a) {
        return eval(left.evaluate(a), right.evaluate(a));
    }

    @Override
    public boolean equals(Object a) {
        if (a == null) return false;
        if (a.getClass() != this.getClass()) {
            return false;
        }
        return ((AbstractOperation) a).left.equals(this.left) && ((AbstractOperation) a).right.equals(this.right);
    }

}
