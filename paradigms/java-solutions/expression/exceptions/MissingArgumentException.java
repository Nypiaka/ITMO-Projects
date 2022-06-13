package expression.exceptions;

public class MissingArgumentException extends ParsingException {
    MissingArgumentException(String message) {
        super("Missing argument at pos: " + message);
    }

}
