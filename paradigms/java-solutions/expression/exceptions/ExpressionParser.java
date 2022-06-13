package expression.exceptions;

import expression.*;
import expression.exceptions.TripleParser;

import java.util.ArrayList;
import java.util.List;

public class ExpressionParser<T> implements TripleParser {
    private Execute<T> parseMake;
    private static int pos = 0;
    private static String expression;
    private static int iterator = 0;

    public void getInfo(Execute<T> a) {
        parseMake = a;
    }

    private boolean hasMore(String body) {
        int k = body.length();
        int iter = 0;
        for (int i = pos + 1; i < expression.length() && iter < body.length(); i++) {
            if (body.charAt(iter) == expression.charAt(i)) {
                k--;
            }
            iter++;
        }
        return k == 0;
    }

    private boolean hasMore(int length) {
        return pos + length < expression.length();
    }

    private static void skipWhitespaces() {
        while (pos < expression.length() && Character.isWhitespace(expression.charAt(pos))) {
            pos++;
        }
    }

    public Arithmetical<T> parse(String expression) throws ParsingException {
        pos = 0;
        iterator = 0;
        ExpressionParser.expression = expression;
        try {
            return parseMaker(this.makeSPList());
        } catch (ParsingException e) {
            throw e;
        }
    }

    private String parseConst() {
        StringBuilder number = new StringBuilder();
        while (pos < expression.length() && Character.isDigit(expression.charAt(pos))) {
            number.append(expression.charAt(pos));
            pos++;
        }
        return number.toString();
    }

    public StringPart[] makeSPList() throws ParsingException {
        int cntrL = 0;
        int cntrR = 0;
        char prevToken = ' ';
        StringPart[] base = new StringPart[11000];
        int iterator = 0;
        while (pos < expression.length()) {
            skipWhitespaces();
            if (pos >= expression.length()) {
                break;
            }
            char current = expression.charAt(pos);
            if (current == '(') {
                cntrL++;
                pos++;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.LEFT, "("));
                iterator++;
            } else if (current == '+') {
                pos++;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.PlUS, "+"));
                iterator++;
            } else if (current == '*') {
                pos++;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.MULTIPLE, "*"));
                iterator++;
            } else if (current == '-') {
                pos++;
                if (prevToken != ')' && !Character.isDigit(prevToken) && prevToken != 'x' && prevToken != 'y' &&
                        prevToken != 'z' && Character.isDigit(expression.charAt(pos))) {
                    base[iterator] = (new StringPart(ArithmeticalExpressionType.CONST, "-" + parseConst()));
                    current = '0';
                    iterator++;
                } else {
                    base[iterator] = (new StringPart(ArithmeticalExpressionType.MINUS, "-"));
                    iterator++;
                }
            } else if (current == '/') {
                pos++;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.DIVIDE, "/"));
                iterator++;
            } else if (current == ')') {
                cntrR++;
                pos++;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.RIGHT, ")"));
                iterator++;
            } else if (current <= '9' && current >= '0') {
                String s = parseConst();
                if (s.length() > 0) base[iterator] = (new StringPart(ArithmeticalExpressionType.CONST, s));
                iterator++;
            } else if ((current == 'x' || current == 'y' || current == 'z')) {
                base[iterator] = (new StringPart(ArithmeticalExpressionType.VARIABLE, Character.toString(current)));
                pos++;
                iterator++;
            } else if (current == 'm') {
                if (hasMore("in")) {
                    if (hasMore(4) && (!Character.isDigit(expression.charAt(pos + 3)))) {
                        pos += 3;
                        base[iterator] = (new StringPart(ArithmeticalExpressionType.MIN, "min"));
                        iterator++;
                    } else throw new WrongArgumentException(pos, "min");
                }
                if (hasMore("ax")) {
                    if (hasMore(4) && (!Character.isDigit(expression.charAt(pos + 3)))) {
                        pos += 3;
                        base[iterator] = (new StringPart(ArithmeticalExpressionType.MAX, "max"));
                        iterator++;
                    } else throw new WrongArgumentException(pos, "max");
                }
            } else if ((current == 'l' || current == 't') && (hasMore("0"))) {
                if (hasMore(3) && (!Character.isLetter(expression.charAt(pos + 2)))) {
                    if (current == 'l') {
                        pos += 2;
                        base[iterator] = (new StringPart(ArithmeticalExpressionType.LEFT_ZERO, "l0"));
                        iterator++;
                    } else {
                        pos += 2;
                        base[iterator] = (new StringPart(ArithmeticalExpressionType.RIGHT_ZERO, "t0"));
                        iterator++;
                    }
                } else throw new WrongArgumentException(pos, current + "0");
            } else if (current == 'c' && hasMore("ount")) {
                pos += 5;
                base[iterator] = (new StringPart(ArithmeticalExpressionType.COUNT, "count"));
                iterator++;
            } else {
                throw new UnsupportedParsingArgumentException(String.valueOf(pos + 1));
            }
            prevToken = current;
        }
        base[iterator] = (new StringPart(ArithmeticalExpressionType.EOF, ""));

        if (cntrL < cntrR) {
            throw new LostLeftBracketException(String.valueOf(pos));
        }
        return base;
    }

    public Arithmetical<T> parseMaker(StringPart[] parts) throws ParsingException {
        StringPart part = parts[iterator];
        iterator++;
        if (part.type == ArithmeticalExpressionType.EOF) {
            return null;
        } else {
            iterator--;
            return thirdPriority(parts);
        }
    }

    public Arithmetical<T> thirdPriority(StringPart[] parts) throws ParsingException {
        Arithmetical<T> value = secondPriority(parts);
        while (true) {
            StringPart part = parts[iterator];
            iterator++;
            switch (part.type) {
                case MIN -> value = new CheckedMin<T>(value, secondPriority(parts));
                case MAX -> value = new CheckedMax<T>(value, secondPriority(parts));
                case EOF, RIGHT -> {
                    iterator--;
                    return value;
                }
                default -> {
                    iterator--;
                    part = parts[iterator];
                    iterator++;
                    if (part.value.equals("")) {
                        throw new MissingArgumentException(String.valueOf(iterator));
                    }
                    throw new WrongArgumentException(iterator, part.value);
                }
            }
        }
    }

    public Arithmetical<T> secondPriority(StringPart[] parts) throws ParsingException {
        Arithmetical<T> value = firstPriority(parts);
        while (true) {
            StringPart part = parts[iterator];
            iterator++;
            switch (part.type) {
                case PlUS -> value = new CheckedAdd<T>(value, firstPriority(parts));
                case MINUS -> value = new CheckedSubtract<T>(value, firstPriority(parts));
                case EOF, RIGHT, MIN, MAX -> {
                    iterator--;
                    return value;
                }
                default -> {
                    iterator--;
                    part = parts[iterator];
                    iterator++;
                    if (part.value.equals("")) {
                        throw new MissingArgumentException(String.valueOf(iterator));
                    }
                    throw new WrongArgumentException(iterator, part.value);
                }
            }
        }
    }

    public Arithmetical<T> firstPriority(StringPart[] parts) throws ParsingException {
        Arithmetical<T> value = zeroPriority(parts);
        while (true) {
            StringPart part = parts[iterator];
            iterator++;
            switch (part.type) {
                case MULTIPLE -> value = new CheckedMultiply<T>(value, zeroPriority(parts));
                case DIVIDE -> value = new CheckedDivide<T>(value, zeroPriority(parts));
                case EOF, RIGHT, PlUS, MINUS, MIN, MAX -> {
                    iterator--;
                    return value;
                }
                default -> {
                    iterator--;
                    part = parts[iterator];
                    iterator++;
                    if (part.value.equals("")) {
                        throw new MissingArgumentException(String.valueOf(iterator));
                    }
                    throw new WrongArgumentException(iterator, part.value);
                }
            }
        }
    }

    public Arithmetical<T> zeroPriority(StringPart[] parts) throws ParsingException {
        StringPart part = parts[iterator];
        iterator++;
        switch (part.type) {
            case COUNT:
                return new Count<>(zeroPriority(parts));
            case MINUS:
                return new CheckedNegate<>(zeroPriority(parts));
            case CONST:
                if (parseMake != null)
                    return new Const<>(parseMake.getVal(part.value));
                else return new Const<>(Integer.parseInt(part.value));
            case VARIABLE:
                return new Variable<>(part.value);
            case LEFT:
                Arithmetical<T> value = thirdPriority(parts);
                part = parts[iterator];
                iterator++;
                if (part.type != ArithmeticalExpressionType.RIGHT) {
                    throw new LostRightBracketException(String.valueOf(iterator));
                }
                return value;
            case LEFT_ZERO:
                return new LeftZero<>(zeroPriority(parts));
            case RIGHT_ZERO:
                return new RightZero<>(zeroPriority(parts));
            default: {
                iterator--;
                part = parts[iterator];
                iterator++;
                if (part.value.equals("")) {
                    throw new MissingArgumentException(String.valueOf(iterator));
                }
                throw new WrongArgumentException(iterator, part.value);
            }
        }
    }
}

