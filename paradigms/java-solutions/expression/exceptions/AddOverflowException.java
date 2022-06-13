package expression.exceptions;

public class AddOverflowException extends ExpressionException {
    public AddOverflowException(String message) {
        super("Overflow at add operation: " + message);
    }
}
