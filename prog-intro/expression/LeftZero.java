package expression;

import java.math.BigDecimal;

public class LeftZero extends AbstractUnaryOperation implements Arithmetical {

    public LeftZero(Arithmetical b) {
        this.currentOperation = b;
        this.leftPart = "l0(";
        this.rightPart = ")";
        this.miniPart = "l0 ";
        this.priority = 7;
    }

    @Override
    public int evaluate(int c) {
        return Integer.numberOfLeadingZeros(currentOperation.evaluate(c));
    }

    @Override
    public BigDecimal evaluate(BigDecimal c) {
        return BigDecimal.ZERO;
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return Integer.numberOfLeadingZeros(currentOperation.evaluate(c, e, f));
    }
}
