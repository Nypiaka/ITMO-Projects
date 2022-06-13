package expression.exceptions;

public class ExecuteInt implements Execute<Integer> {
    boolean flag = false;

    public ExecuteInt(boolean flag) {
        this.flag = flag;
    }

    @Override
    public Integer add(Integer c, Integer d) {
        if (flag) {
            if (c > 0 && d > 0) {
                if (Integer.MAX_VALUE - c < d) {
                    throw new AddOverflowException("");
                }
            }
            if (c < 0 && d < 0) {
                if (Integer.MIN_VALUE - c > d) {
                    throw new AddOverflowException("");
                }
            }
        }
        return c + d;
    }

    @Override
    public Integer mul(Integer c, Integer d) {
        if (flag) {
            if (c != -1 && d != -1) {
                if (c > 0 && d > 0) {
                    if (Integer.MAX_VALUE / c < d)
                        throw new MultipyOverflowException("");
                } else if (c > 0 && d < 0) {
                    if (Integer.MIN_VALUE / d < c)
                        throw new MultipyOverflowException("");
                } else if (c < 0 && d > 0) {
                    if (Integer.MIN_VALUE / c < d)
                        throw new MultipyOverflowException("");
                } else if (c < 0 && d < 0) {
                    if (Integer.MAX_VALUE / d > c)
                        throw new MultipyOverflowException("");
                }
            } else {
                if (c == -1) {
                    if (d == Integer.MIN_VALUE)
                        throw new MultipyOverflowException("");
                }
                if (d == -1) {
                    if (c == Integer.MIN_VALUE)
                        throw new MultipyOverflowException("");
                }
            }
        }
        return c * d;
    }

    @Override
    public Integer sub(Integer c, Integer d) {
        if (flag) {
            if (c <= 0 && d >= 0) {
                if (Integer.MIN_VALUE + d > c) {
                    throw new SubtractOverflowException("");
                }
            } else if (c >= 0 && d <= 0) {
                if (c > Integer.MAX_VALUE + d) {
                    throw new SubtractOverflowException("");
                }
            }
        }
        return c - d;
    }

    @Override
    public Integer div(Integer c, Integer d) {
        if (flag) {
            if (c == Integer.MIN_VALUE && d == -1) {
                throw new DivideOverflowException("");
            }
            if (d == 0) {
                throw new DivisionByZeroDivideException("");
            }
        }
        return c / d;
    }

    @Override
    public Integer min(Integer c, Integer d) {
        if (c > d) {
            return d;
        }
        return c;
    }

    @Override
    public Integer max(Integer l, Integer r) {
        return Math.max(l, r);
    }

    @Override
    public Integer neg(Integer a) {
        if (flag) {
            if (a == Integer.MIN_VALUE) throw new NegateOverflowException("");
        }
        return -a;
    }

    @Override
    public Integer getVal(String val) {
        return Integer.parseInt(val);
    }

    @Override
    public Integer count(Integer a) {
        return Integer.bitCount(a);
    }
}
