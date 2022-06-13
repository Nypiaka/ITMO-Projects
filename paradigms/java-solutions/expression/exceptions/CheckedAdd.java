package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedAdd<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public CheckedAdd(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 5;
        this.operationToString = "+";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " + " + right.toString() + ")";
    }

    @Override
    protected T eval(T x, T y, Execute<T> m) {
        return m.add(x, y);
    }

    @Override
    protected int eval(int c, int d) {
        if (c > 0 && d > 0) {
            if (Integer.MAX_VALUE - c < d) {
                throw new AddOverflowException(left + " + " + right);
            }
        }
        if (d < 0 && c < 0) {
            if (Integer.MIN_VALUE - c > d) {
                throw new AddOverflowException(left + " + " + right);
            }
        }
        return c + d;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(2);
    }

}
