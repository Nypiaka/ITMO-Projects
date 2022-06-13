package expression.exceptions;

import expression.AbstractUnaryOperation;
import expression.Arithmetical;

public class RightZero<T> extends AbstractUnaryOperation<T> implements Arithmetical<T> {

    @Override
    protected T eval(T x, Execute<T>m) {
        return null;
    }

    @Override
    protected int eval(int x) {
        return 0;
    }

    public RightZero(Arithmetical<T> b) {
        this.currentOperation = b;
        this.leftPart = "t0(";
        this.rightPart = ")";
        this.miniPart = "t0 ";
        this.priority = 7;
    }

    @Override
    public int evaluate(int c) {
        return Integer.numberOfTrailingZeros(currentOperation.evaluate(c));
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return Integer.numberOfTrailingZeros(currentOperation.evaluate(c, e, f));
    }

}