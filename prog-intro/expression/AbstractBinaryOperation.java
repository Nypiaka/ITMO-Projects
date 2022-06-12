package expression;

import java.math.BigDecimal;
import java.util.Objects;

public abstract class AbstractBinaryOperation implements Arithmetical {
    Arithmetical left;
    Arithmetical right;
    int priority;
    String operationToString;

    @Override
    public int returnPriority() {
        return priority;
    }

    abstract int eval(int x, int y);

    abstract BigDecimal eval(BigDecimal x, BigDecimal y);

    @Override
    public int hashCode() {
        return Objects.hash(left, right, this.getClass());
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
        return ((AbstractBinaryOperation) a).left.equals(this.left) && ((AbstractBinaryOperation) a).right.equals(this.right);
    }

    public String toMiniStringBuilder(int quest) {
        String sa = left.toMiniString();
        String sb = right.toMiniString();
        if (left.returnPriority() < this.priority) {
            sa = "(" + sa + ")";
        }
        if (quest == 1) {
            if (right.returnPriority() <= this.priority) {
                sb = "(" + sb + ")";
            }
        } else if (quest == 2) {
            if (right.returnPriority() < this.priority) {
                sb = "(" + sb + ")";
            }
        } else {
            if (!right.getClass().equals(this.getClass())) {
                if (right.returnPriority() <= this.priority) {
                    sb = "(" + sb + ")";
                }
            } else {
                if (right.returnPriority() < this.priority) {
                    sb = "(" + sb + ")";
                }
            }
        }
        return sa + " " + this.operationToString + " " + sb;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(1);
    }

}
