package expression;

import java.math.BigDecimal;
import java.util.Objects;

public class Variable implements Arithmetical {
    String a;

    public Variable(String c) {
        a = c;
    }

    @Override
    public int evaluate(int c) {
        return c;
    }

    @Override
    public int returnPriority() {
        return 9;
    }

    @Override
    public BigDecimal evaluate(BigDecimal c) {
        return c;
    }

    @Override
    public int evaluate(int c, int e, int f) {
        return switch (a) {
            case "x" -> c;
            case "y" -> e;
            case "z" -> f;
            default -> -1;
        };
    }


    @Override
    public String toString() {
        return a;
    }

    @Override
    public String toMiniString() {
        return a;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.a.hashCode());
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass())
            return this.a.equals(((Variable) object).a);
        return false;
    }
}
