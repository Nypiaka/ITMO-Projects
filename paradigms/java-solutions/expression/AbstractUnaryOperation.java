package expression;

import expression.exceptions.Execute;

import java.util.Objects;

public abstract class AbstractUnaryOperation<T> implements Arithmetical<T> {
    public Execute<T> maker;
    public Arithmetical<T> currentOperation;
    public String leftPart;
    public String rightPart;
    public String miniPart;
    public int priority;

    abstract protected T eval(T x, Execute<T> maker);

    abstract protected int eval(int x);


    @Override
    public String toString() {
        return leftPart + this.currentOperation.toString() + rightPart;
    }

    public T evaluate(T a, Execute<T> maker) {
        return eval(currentOperation.evaluate(a, maker), maker);
    }

    public T evaluate(T a, T b, T c, Execute<T> maker) {
        return eval(currentOperation.evaluate(a, b, c, maker), maker);
    }

    @Override
    public int returnPriority() {
        return priority;
    }

    @Override
    public int evaluate(int a, int b, int c) {
        return eval(currentOperation.evaluate(a, b, c));
    }

    @Override
    public int evaluate(int x) {
        return eval(currentOperation.evaluate(x));
    }

    @Override
    public String toMiniString() {
        if (currentOperation.returnPriority() >= this.priority) {
            return miniPart + currentOperation.toMiniString();
        } else return leftPart + currentOperation.toMiniString() + rightPart;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.getClass(), currentOperation.hashCode());
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass())
            return this.currentOperation == ((AbstractUnaryOperation) object).currentOperation;
        return false;

    }

}
