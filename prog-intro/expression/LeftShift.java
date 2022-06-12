package expression;

import java.math.BigDecimal;

public class LeftShift extends AbstractBinaryOperation implements Arithmetical {
    public LeftShift(Arithmetical left, Arithmetical right) {
        this.left = left;
        this.right = right;
        this.priority = 4;
        this.operationToString = "<<";
    }

    @Override
    public String toString() {
        return "(" + left.toString() + " << " + right.toString() + ")";
    }

    @Override
    public int eval(int c, int d) {
        return c << d;
    }

    @Override
    public BigDecimal eval(BigDecimal x, BigDecimal y) {
        return null;
    }


}
