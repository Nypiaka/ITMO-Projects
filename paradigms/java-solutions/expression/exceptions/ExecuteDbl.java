package expression.exceptions;

public class ExecuteDbl implements Execute<Double> {

    @Override
    public Double add(Double c, Double d) {
        return c + d;
    }

    @Override
    public Double mul(Double c, Double d) {
        return c * d;
    }

    @Override
    public Double sub(Double c, Double d) {
        return c - d;
    }

    @Override
    public Double div(Double c, Double d) {
        return c / d;
    }

    @Override
    public Double min(Double c, Double d) {
        return Math.min(c, d);
    }

    @Override
    public Double max(Double l, Double r) {
        return Math.max(l, r);
    }

    @Override
    public Double neg(Double a) {
        return -a;
    }

    @Override
    public Double getVal(String val) {
        return Double.valueOf(val);
    }

    @Override
    public Double count(Double a) {
        return (double) Long.bitCount(Double.doubleToLongBits(a));
    }
}
