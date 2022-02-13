package expression;

import java.math.BigDecimal;

public class RightZero extends AbstractUnaryOperation implements Arithmetical {

    public RightZero(Arithmetical b) {
        this.currentOperation = b;
        this.leftPart = "t0(";
        this.rightPart = ")";
        this.miniPart = "t0 ";
        this.priority = 7;
    }

    @Override
    public int evaluate(int c) {
        return Integer.numberOfTrailingZeros(currentOperation.evaluate(c));
    }

    @Override
    public BigDecimal evaluate(BigDecimal c) {
        return BigDecimal.ZERO;
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return Integer.numberOfTrailingZeros(currentOperation.evaluate(c, e, f));
    }


}