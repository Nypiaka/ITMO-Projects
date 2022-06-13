package expression.exceptions;

public class LostRightBracketException extends ParsingException {
    LostRightBracketException(String message) {
        super("Lost right bracket at pos: " + message);
    }
}