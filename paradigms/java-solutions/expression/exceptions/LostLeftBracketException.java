package expression.exceptions;

public class LostLeftBracketException extends ParsingException {
    LostLeftBracketException(String message) {
        super("Lost left bracket for right bracket at pos: " + message);
    }
}
