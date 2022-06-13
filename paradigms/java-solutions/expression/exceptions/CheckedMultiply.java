package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedMultiply<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public CheckedMultiply(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 6;
        this.operationToString = "*";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " * " + right.toString() + ")";
    }

    @Override
    public T eval(T c, T d, Execute<T> m) {
        return m.mul(c, d);
    }

    @Override
    protected int eval(int c, int d) {
            if (c != -1 && d != -1) {
                if (c > 0 && d > 0) {
                    if (Integer.MAX_VALUE / c < d)
                        throw new MultipyOverflowException(left + " * " + right);
                } else if (c > 0 && d < 0) {
                    if (Integer.MIN_VALUE / d < c)
                        throw new MultipyOverflowException(left + " * " + right);
                } else if (c < 0 && d > 0) {
                    if (Integer.MIN_VALUE / c < d)
                        throw new MultipyOverflowException(left + " * " + right);
                } else if (c < 0 && d < 0) {
                    if (Integer.MAX_VALUE / d > c)
                        throw new MultipyOverflowException(left + " * " + right);
                }
            } else {
                if (c == -1) {
                    if (d == Integer.MIN_VALUE)
                        throw new MultipyOverflowException(left + " * " + right);
                }
                if (d == -1) {
                    if (c == Integer.MIN_VALUE)
                        throw new MultipyOverflowException(left + " * " + right);
                }
            }
        return c * d;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(3);
    }

}
