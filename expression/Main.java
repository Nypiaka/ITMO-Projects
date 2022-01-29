package expression;

public class Main {
    public static void main(String[] args) {
        var vv = new Add(
                new Subtract(
                        new Multiply(
                                new Variable("x"),
                                new Variable("x")),
                        new Multiply(
                                new Const(2),
                                new Variable("x"))),
                new Const(1));
        System.out.println(vv.toString());
        System.out.println(vv.toMiniString());
        System.out.println(vv.evaluate(Integer.parseInt("2")));
    }
}
