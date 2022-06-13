package expression.exceptions;

import java.math.BigInteger;

public class ExecuteBI implements Execute<BigInteger> {

    @Override
    public BigInteger add(BigInteger l, BigInteger r) {
        return l.add(r);
    }

    @Override
    public BigInteger mul(BigInteger l, BigInteger r) {
        return l.multiply(r);
    }

    @Override
    public BigInteger sub(BigInteger l, BigInteger r) {
        return l.subtract(r);
    }

    @Override
    public BigInteger div(BigInteger l, BigInteger r) {
        if (r.equals(BigInteger.ZERO)) {
            throw new DivisionByZeroDivideException("");
        }
        return l.divide(r);
    }

    @Override
    public BigInteger min(BigInteger l, BigInteger r) {
        return l.compareTo(r) > 0 ? r : l;
    }

    @Override
    public BigInteger max(BigInteger l, BigInteger r) {
        return l.compareTo(r) < 0 ? r : l;
    }

    @Override
    public BigInteger neg(BigInteger a) {
        return a.multiply(BigInteger.valueOf(-1));
    }

    @Override
    public BigInteger getVal(String val) {
        return new BigInteger(val);
    }

    @Override
    public BigInteger count(BigInteger a) {
        return new BigInteger(Integer.toString(a.bitCount()));
    }
}
