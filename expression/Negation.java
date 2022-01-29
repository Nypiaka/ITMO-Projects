package expression;

import java.math.BigDecimal;

public class Negation extends AbstractUnaryOperation implements Arithmetical {

    public Negation(Arithmetical b) {
        this.currentOperation = b;
        this.leftPart = "-(";
        this.rightPart = ")";
        this.miniPart = "- ";
        this.priority = 7;
    }


    @Override
    public int evaluate(int c) {
        return -currentOperation.evaluate(c);
    }

    @Override
    public BigDecimal evaluate(BigDecimal c) {
        return currentOperation.evaluate(c).multiply(BigDecimal.valueOf(-1));
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return -currentOperation.evaluate(c, e, f);
    }

}
