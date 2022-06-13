package expression.exceptions;

public class NegateOverflowException extends ExpressionException {
    public NegateOverflowException(String message) {
        super("Overflow at negate operation: -" + message);
    }
}
