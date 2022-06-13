package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedDivide<T> extends AbstractBinaryOperation<T> {

    public CheckedDivide(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 6;
        this.operationToString = "/";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " / " + right.toString() + ")";
    }

    @Override
    protected T eval(T x, T y, Execute<T> m) {
        return m.div(x, y);
    }

    @Override
    protected int eval(int c, int d) {
        if (c == Integer.MIN_VALUE && d == -1) {
            throw new DivideOverflowException(left + " / " + right);
        }
        if (d == 0) {
            throw new DivisionByZeroDivideException(left + " / " + right);
        }
        return c / d;
    }
}
