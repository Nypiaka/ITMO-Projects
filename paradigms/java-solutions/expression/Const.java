package expression;

import expression.exceptions.Execute;

public class Const<T> implements Arithmetical<T> {
    T a;
    int a1;


    @Override
    public int returnPriority() {
        return 9;
    }

    public Const(T b) {
        a = b;
    }

    public Const(int b) {
        a1 = b;
    }

    @Override
    public T evaluate(T c, Execute<T> m) {
        return a;
    }

    @Override
    public T evaluate(T c, T e, T f, Execute<T> m) {
        return a;
    }

    @Override
    public String toString() {
        if (a != null)
            return a.toString();
        return String.valueOf(a1);
    }

    @Override
    public String toMiniString() {
        if (a!=null)
        return a.toString();
        return String.valueOf(a1);
    }

    @Override
    public int hashCode() {
        return a.hashCode();
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass()) {
            return this.a == ((Const) object).a;
        }
        return false;

    }

    @Override
    public int evaluate(int x) {
        return a1;
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return a1;
    }
}
