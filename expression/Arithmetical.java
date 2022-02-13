package expression;


import java.math.BigDecimal;

public interface Arithmetical {
    int evaluate(int x);
    String toMiniString();
    int evaluate(int x, int y, int z);

    BigDecimal evaluate(BigDecimal x);

    int returnPriority();
}
