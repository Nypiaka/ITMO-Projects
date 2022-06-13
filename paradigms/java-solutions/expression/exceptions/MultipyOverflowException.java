package expression.exceptions;

public class MultipyOverflowException extends ExpressionException {
    public MultipyOverflowException(String message) {
        super("Overflow at multiply operation: " + message);
    }
}
