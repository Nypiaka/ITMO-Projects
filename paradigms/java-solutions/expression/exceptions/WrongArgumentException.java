package expression.exceptions;

public class WrongArgumentException extends ParsingException {
    WrongArgumentException(String message) {
        super("Wrong expression part at pose: " + message);
    }

    WrongArgumentException(int pose, String message) {
        super("Wrong expression part at pose: " + pose + ": " + message);
    }
}
