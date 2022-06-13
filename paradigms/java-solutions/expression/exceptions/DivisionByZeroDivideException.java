package expression.exceptions;

public class DivisionByZeroDivideException extends ExpressionException {
    public DivisionByZeroDivideException(String message) {
        super("Division by zero at divide operation: " + message);
    }
}
