package expression.generic;

import expression.Arithmetical;
import expression.exceptions.*;

import java.util.Map;

public class GenericTabulator implements Tabulator {

    @Override
    public Object[][][] tabulate(String mode, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws Exception {
        Execute<? extends Number> maker;
        switch (mode) {
            case "i" -> maker = new ExecuteInt(true);
            case "d" -> maker = new ExecuteDbl();
            case "bi" -> maker = new ExecuteBI();
            case "u" -> maker = new ExecuteInt(false);
            case "l" -> maker = new ExecuteLong();
            case "s" -> maker = new ExecuteShort();
            default -> throw new IllegalArgumentException("Wrong mode");
        }
        return getTable(maker, expression, x1, x2, y1, y2, z1, z2);
    }

    public <T extends Number> Object[][][] getTable(Execute<T> maker, String expression, int x1, int x2, int y1, int y2, int z1, int z2) throws ParsingException {
        ExpressionParser<T> parser = new ExpressionParser<>();
        parser.getInfo(maker);
        Arithmetical<T> exp;
        exp = parser.parse(expression);
        Object[][][] res = new Object[x2 - x1 + 1][y2 - y1 + 1][z2 - z1 + 1];
        for (int i = x1; i <= x2; i++) {
            for (int j = y1; j <= y2; j++) {
                for (int k = z1; k <= z2; k++) {
                    T x = maker.getVal(Integer.toString(i));
                    T y = maker.getVal(Integer.toString(j));
                    T z = maker.getVal(Integer.toString(k));
                    try {
                        res[i - x1][j - y1][k - z1] = exp.evaluate(x, y, z, maker);
                    } catch (Exception e) {
                        res[i - x1][j - y1][k - z1] = null;
                    }
                }
            }
        }
        return res;
    }
}
