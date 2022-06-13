package expression.exceptions;

public class SubtractOverflowException extends ExpressionException {
    public SubtractOverflowException(String message) {
        super("Overflow at subtract operation: " + message);
    }
}
