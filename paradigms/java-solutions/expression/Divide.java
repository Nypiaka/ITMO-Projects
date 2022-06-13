package expression;

import expression.exceptions.Execute;

public class Divide<T> extends AbstractBinaryOperation<T> implements Arithmetical<T> {

    public Divide(Arithmetical<T> left, Arithmetical<T> right) {
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
    protected int eval(int a, int b) {
        return a / b;
    }


}
