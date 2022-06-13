package expression;

import expression.exceptions.Execute;

import java.util.Objects;

public class Variable<T> implements Arithmetical<T> {
    String a;

    public Variable(String c) {
        a = c;
    }

    @Override
    public int evaluate(int c) {
        return c;
    }

    @Override
    public T evaluate(T x, T y, T z, Execute<T> m) {
        return switch (a) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> null;
        };
    }

    @Override
    public T evaluate(T x, Execute<T> m) {
        return x;
    }

    @Override
    public int returnPriority() {
        return 9;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return switch (a) {
            case "x" -> x;
            case "y" -> y;
            case "z" -> z;
            default -> throw new IllegalArgumentException("wrong letter: " + a);
        };
    }


    @Override
    public String toString() {
        return a;
    }

    @Override
    public String toMiniString() {
        return a;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.a.hashCode());
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass())
            return this.a.equals(((Variable) object).a);
        return false;
    }
}
