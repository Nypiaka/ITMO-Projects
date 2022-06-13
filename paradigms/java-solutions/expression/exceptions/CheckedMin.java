package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedMin<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {
    public CheckedMin(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 4;
        this.operationToString = "min";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " min " + right.toString() + ")";
    }

    @Override
    public T eval(T c, T d, Execute<T> m) {
        return m.min(c, d);
    }

    @Override
    protected int eval(int a, int b) {
        return Math.min(a, b);
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(3);
    }

}
