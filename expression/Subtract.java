package expression;

import java.math.BigDecimal;

public class Subtract extends AbstractBinaryOperation implements Arithmetical {

    public Subtract(Arithmetical left, Arithmetical right) {
        this.left = left;
        this.right = right;
        this.priority = 5;
        this.operationToString = "-";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " - " + right.toString() + ")";
    }

    @Override
    public int eval(int c, int d) {
        return c - d;
    }

    @Override
    public BigDecimal eval(BigDecimal c, BigDecimal d) {
        return c.subtract(d);
    }

}
