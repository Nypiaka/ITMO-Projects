package expression.exceptions;

import expression.AbstractUnaryOperation;
import expression.Arithmetical;

public class CheckedNegate<T> extends AbstractUnaryOperation<T> implements Arithmetical<T> {

    public CheckedNegate(Arithmetical<T> b) {
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
    protected int eval(int a) {
        if (a == Integer.MIN_VALUE) throw new NegateOverflowException(currentOperation.toString());
        return -a;
    }

}
