package expression;

import expression.exceptions.Execute;

public interface Arithmetical<T> extends ToMiniString, Expression, TripleExpression {

    T evaluate(T x, T y, T z, Execute<T> a);

    T evaluate(T x, Execute<T> a);

    int evaluate(int a, int b, int c);

    int evaluate(int a);

    int returnPriority();

}
