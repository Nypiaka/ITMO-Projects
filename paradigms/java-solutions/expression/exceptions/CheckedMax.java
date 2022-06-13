package expression.exceptions;

import expression.AbstractBinaryOperation;
import expression.Arithmetical;

public class CheckedMax<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {
    public CheckedMax(Arithmetical<T> left, Arithmetical<T> right) {
        this.left = left;
        this.right = right;
        this.priority = 4;
        this.operationToString = "max";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " max " + right.toString() + ")";
    }

    @Override
    public T eval(T c, T d, Execute<T> m) {
        return m.max(c, d);
    }

    @Override
    protected int eval(int a, int b) {
        return Math.max(a, b);
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(3);
    }

}