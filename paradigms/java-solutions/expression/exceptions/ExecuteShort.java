package expression.exceptions;

public class ExecuteShort implements Execute<Short> {
    @Override
    public Short add(Short l, Short r) {
        return (short) (l + r);
    }

    @Override
    public Short mul(Short l, Short r) {
        return (short) (l * r);
    }

    @Override
    public Short sub(Short l, Short r) {
        return (short) (l - r);
    }

    @Override
    public Short div(Short l, Short r) {
        return (short) (l / r);
    }

    @Override
    public Short min(Short l, Short r) {
        return (short) (Math.min(l, r));
    }

    @Override
    public Short max(Short l, Short r) {
        return (short) (Math.max(l, r));
    }

    @Override
    public Short neg(Short a) {
        return (short) (-a);
    }

    @Override
    public Short getVal(String value) {
        return (short) Integer.parseInt(value);
    }

    @Override
    public Short count(Short a) {
        return (short) Integer.bitCount(a & 0xffff);
    }
}
