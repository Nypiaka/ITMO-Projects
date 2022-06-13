package expression;

import expression.exceptions.Execute;

public class Add<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public Add(Arithmetical<T> left, Arithmetical<T> right) {
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
    protected int eval(int x, int y) {
        return x + y;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(2);
    }

}
