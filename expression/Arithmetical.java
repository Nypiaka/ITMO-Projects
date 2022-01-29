package expression;


import java.math.BigDecimal;

public interface Arithmetical extends ToMiniString, Expression, TripleExpression, BigDecimalExpression {
    int evaluate(int x);

    int evaluate(int x, int y, int z);

    BigDecimal evaluate(BigDecimal x);

    int returnPriority();
}
