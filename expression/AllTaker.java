package expression;

public interface AllTaker {
    Subtract subtract = new Subtract(new Variable("x"),new Variable("x"));
    Multiply multiply = new Multiply(new Variable("x"),new Variable("x"));
    Divide divide = new Divide(new Variable("x"),new Variable("x"));
    Variable variable = new Variable("x");
    Const constn = new Const(1);
    Negation neg = new Negation(new Const(0));
    RightShift rightsh = new RightShift(new Variable("x"), new Variable("x"));
    LeftShift leftsh = new LeftShift(new Variable("x"), new Variable("x"));
    RightArithmShift aright = new RightArithmShift(new Variable("x"), new Variable("x"));
    RightZero rz = new RightZero(new Variable("x"));
    LeftZero lz = new LeftZero(new Variable("x"));
}
