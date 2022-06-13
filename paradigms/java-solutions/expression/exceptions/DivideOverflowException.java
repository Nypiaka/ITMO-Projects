package expression.exceptions;

public class DivideOverflowException extends ExpressionException {
    public DivideOverflowException(String message) {
        super("Overflow at divide operation: " + message);
    }
}
