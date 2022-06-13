package expression.exceptions;

import expression.AbstractUnaryOperation;
import expression.Arithmetical;

public class Count<T> extends AbstractUnaryOperation<T> implements Arithmetical<T> {


    public Count(Arithmetical<T> b) {
        this.currentOperation = b;
        this.leftPart = "count(";
        this.rightPart = ")";
        this.miniPart = "count ";
        this.priority = 7;
    }

    @Override
    public T eval(T x, Execute<T> m) {
        return m.count(x);
    }

    @Override
    protected int eval(int a) {
        return Integer.bitCount(a);
    }

}
