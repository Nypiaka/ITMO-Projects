package expression.exceptions;

import expression.AbstractUnaryOperation;
import expression.Arithmetical;

public class LeftZero<T> extends AbstractUnaryOperation<T> implements Arithmetical<T> {

    @Override
    protected T eval(T x, Execute<T> m) {
        return null;
    }

    @Override
    protected int eval(int x) {
        return 0;
    }

    public LeftZero(Arithmetical<T> b) {
        this.currentOperation = b;
        this.leftPart = "l0(";
        this.rightPart = ")";
        this.miniPart = "l0 ";
        this.priority = 7;
    }

    @Override
    public int evaluate(int c) {
        return Integer.numberOfLeadingZeros(currentOperation.evaluate(c));
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return Integer.numberOfLeadingZeros(currentOperation.evaluate(c, e, f));
    }
}
