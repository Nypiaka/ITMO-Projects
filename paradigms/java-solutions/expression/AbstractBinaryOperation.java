package expression;

import expression.exceptions.Execute;

import java.util.Objects;

public abstract class AbstractBinaryOperation<T> implements Arithmetical<T> {
    public Execute<T> maker;
    public Arithmetical<T> left;
    public Arithmetical<T> right;
    public int priority;
    public String operationToString;

    abstract protected T eval(T x, T y, Execute<T> maker);

    abstract protected int eval(int a, int b);

    public T evaluate(T a, Execute<T> maker) {
        return eval(left.evaluate(a, maker), right.evaluate(a, maker), maker);
    }

    public T evaluate(T a, T b, T c, Execute<T> maker) {
        return eval(left.evaluate(a, b, c, maker), right.evaluate(a, b, c, maker), maker);
    }

    @Override
    public int evaluate(int a, int b, int c) {
        return eval(left.evaluate(a, b, c), right.evaluate(a, b, c));
    }

    @Override
    public int evaluate(int a) {
        return eval(left.evaluate(a), right.evaluate(a));
    }

    @Override
    public int returnPriority() {
        return priority;
    }

    @Override
    public int hashCode() {
        return Objects.hash(left, right, this.getClass());
    }

    @Override
    public boolean equals(Object a) {
        if (a == null) return false;
        if (a.getClass() != this.getClass()) {
            return false;
        }
        return ((AbstractBinaryOperation) a).left.equals(this.left) && ((AbstractBinaryOperation) a).right.equals(this.right);
    }

    public String toMiniStringBuilder(int quest) {
        String sa = left.toMiniString();
        String sb = right.toMiniString();
        if (left.returnPriority() < this.priority) {
            sa = "(" + sa + ")";
        }
        if (quest == 1) {
            if (right.returnPriority() <= this.priority) {
                sb = "(" + sb + ")";
            }
        } else if (quest == 2) {
            if (right.returnPriority() < this.priority) {
                sb = "(" + sb + ")";
            }
        } else {
            if (!right.getClass().equals(this.getClass())) {
                if (right.returnPriority() <= this.priority) {
                    sb = "(" + sb + ")";
                }
            } else {
                if (right.returnPriority() < this.priority) {
                    sb = "(" + sb + ")";
                }
            }
        }
        return sa + " " + this.operationToString + " " + sb;
    }

    @Override
    public String toMiniString() {
        return toMiniStringBuilder(1);
    }

}
