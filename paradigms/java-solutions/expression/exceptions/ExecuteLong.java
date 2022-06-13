package expression.exceptions;

public class ExecuteLong implements Execute<Long> {
    @Override
    public Long add(Long l, Long r) {
        return l + r;
    }

    @Override
    public Long mul(Long l, Long r) {
        return l * r;
    }

    @Override
    public Long sub(Long l, Long r) {
        return l - r;
    }

    @Override
    public Long div(Long l, Long r) {
        return l / r;
    }

    @Override
    public Long min(Long l, Long r) {
        return Math.min(l, r);
    }

    @Override
    public Long max(Long l, Long r) {
        return Math.max(l, r);
    }

    @Override
    public Long neg(Long a) {
        return -a;
    }

    @Override
    public Long getVal(String value) {
        return Long.parseLong(value);
    }

    @Override
    public Long count(Long a) {
        return (long) Long.bitCount(a);
    }
}
