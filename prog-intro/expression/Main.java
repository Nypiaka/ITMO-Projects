package expression;

import expression.parser.ExpressionParser;

public class Main {
    public static void main(String[] args) {
        String s = "x*(x+y)-((7*z)+(x+y))/9";
        ExpressionParser parser = new ExpressionParser();
        System.out.println(parser.parse(s).evaluate(10, 10, 10));
        System.out.println(parser.parse(s).toMiniString());
    }
}
