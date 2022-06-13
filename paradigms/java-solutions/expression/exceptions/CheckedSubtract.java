package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedSubtract<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public CheckedSubtract(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 5;
        this.operationToString = "-";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " - " + right.toString() + ")";
    }

    @Override
    public T eval(T c, T d, Execute<T> m) {
        return m.sub(c, d);
    }

    @Override
    protected int eval(int c, int d) {
        if (c <= 0 && d >= 0) {
            if (Integer.MIN_VALUE + d > c) {
                throw new SubtractOverflowException(left + " - " + right);
            }
        } else if (c >= 0 && d <= 0) {
            if (c > Integer.MAX_VALUE + d) {
                throw new SubtractOverflowException(left + " - " + right);
            }
        }
        return c - d;
    }

}
