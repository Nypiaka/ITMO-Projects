package expression.exceptions;

public class UnsupportedParsingArgumentException extends ParsingException {
    UnsupportedParsingArgumentException(String message) {
        super("Unsupported parsing argument at pos: " + message);
    }
}
