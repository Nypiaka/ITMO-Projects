package expression;

import expression.exceptions.Execute;

public class Multiply<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public Multiply(Arithmetical<T> left, Arithmetical<T> right) {
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
    protected int eval(int a, int b) {
        return a * b;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(3);
    }

}
