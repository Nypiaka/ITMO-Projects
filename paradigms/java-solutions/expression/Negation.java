package expression;

import expression.exceptions.Execute;

public class Negation<T> extends AbstractUnaryOperation<T> implements Arithmetical<T> {

    public Negation(Arithmetical<T> b) {
        this.currentOperation = b;
        this.leftPart = "-(";
        this.rightPart = ")";
        this.miniPart = "- ";
        this.priority = 7;
    }

    @Override
    public T eval(T x, Execute<T> m) {
        return m.neg(x);
    }

    @Override
    protected int eval(int x) {
        return -x;
    }

    @Override
    public int evaluate(int x) {
        return 0;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return 0;
    }
}
