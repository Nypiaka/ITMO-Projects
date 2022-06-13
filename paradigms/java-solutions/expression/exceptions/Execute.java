package expression.exceptions;

public interface Execute<T> {

    T add(T l, T r);

    T mul(T l, T r);

    T sub(T l, T r);

    T div(T l, T r);

    T min(T l, T r);

    T max(T l, T r);

    T neg(T a);

    T getVal(String value);

    T count(T a);
}
