package expression.parser;

import expression.*;

import java.util.ArrayList;
import java.util.List;

public class ExpressionParser {
    private static int pos = 0;
    private static String expression;


    private static void skipWhitespaces() {
        while (pos < expression.length() && Character.isWhitespace(expression.charAt(pos))) {
            pos++;
        }
    }

    public Arithmetical parse(String expression) {
        pos = 0;
        ExpressionParser.expression = expression;
        return parseMaker(new StringPartBuffer(this.makeSPList()));
    }

    public List<StringPart> makeSPList() {
        List<StringPart> base = new ArrayList<>();
        while (pos < expression.length()) {
            skipWhitespaces();
            if (pos >= expression.length()) {
                break;
            }
            char cur = expression.charAt(pos);
            if (cur == '(') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.LEFT, "("));
            } else if (cur == '+') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.PlUS, "+"));
            } else if (cur == '*') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.MULTIPLE, "*"));
            } else if (cur == '-') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.MINUS, "-"));
            } else if (cur == '/') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.DIVIDE, "/"));
            } else if (cur == '<') {
                pos += 2;
                base.add(new StringPart(ArithmeticalExpressionType.LEFT_SHIFT, "<<"));
            } else if (cur == '>' && expression.charAt(pos + 1) == '>' && expression.charAt(pos + 2) != '>') {
                pos += 2;
                base.add(new StringPart(ArithmeticalExpressionType.RIGHT_SHIFT, ">>"));
            } else if (cur == '>' && expression.charAt(pos + 1) == '>' && expression.charAt(pos + 2) == '>') {
                pos += 3;
                base.add(new StringPart(ArithmeticalExpressionType.ARITHMETICAL_RIGHT_SHIFT, ">>>"));
            } else if (cur == ')') {
                pos++;
                base.add(new StringPart(ArithmeticalExpressionType.RIGHT, ")"));
            } else if (cur <= '9' && cur >= '0') {
                StringBuilder number = new StringBuilder();
                int y = pos;
                while (pos < expression.length() && expression.charAt(pos) <= '9' && expression.charAt(pos) >= '0') {
                    number.append(expression.charAt(pos));
                    pos++;
                }
                if (!number.toString().equals("0") && y > 0 && expression.charAt(y - 1) == '-' && base.size() > 0 &&
                        base.get(base.size() - 1).type == ArithmeticalExpressionType.MINUS &&
                        (base.size() == 1 || (base.get(base.size() - 2).type != ArithmeticalExpressionType.CONST &&
                                base.get(base.size() - 2).type != ArithmeticalExpressionType.VARIABLE &&
                                base.get(base.size() - 2).type != ArithmeticalExpressionType.RIGHT))) {
                    base.get(base.size() - 1).type = ArithmeticalExpressionType.CONST;
                    base.get(base.size() - 1).value = "-" + number;
                } else {
                    base.add(new StringPart(ArithmeticalExpressionType.CONST, number.toString()));
                }
            } else if (Character.isLetter(cur) && cur != 'l' && cur != 't') {
                StringBuilder var = new StringBuilder();
                while (pos < expression.length() && Character.isLetter(expression.charAt(pos))) {
                    var.append(expression.charAt(pos));
                    pos++;
                }
                base.add(new StringPart(ArithmeticalExpressionType.VARIABLE, var.toString()));
            } else if (cur == 'l' || cur == 't') {
                if (cur == 'l') {
                    pos += 2;
                    base.add(new StringPart(ArithmeticalExpressionType.LEFT_ZERO, "l0"));
                } else {
                    pos += 2;
                    base.add(new StringPart(ArithmeticalExpressionType.RIGHT_ZERO, "t0"));
                }
            }
        }
        base.add(new StringPart(ArithmeticalExpressionType.EOF, ""));
        return base;
    }

    public static Arithmetical parseMaker(StringPartBuffer parts) {
        StringPart part = parts.next();
        if (part.type == ArithmeticalExpressionType.EOF) {
            return null;
        } else {
            parts.back();
            return thirdPriority(parts);
        }
    }

    public static Arithmetical thirdPriority(StringPartBuffer parts) {
        Arithmetical value = secondPriority(parts);
        while (true) {
            StringPart part = parts.next();
            switch (part.type) {
                case LEFT_SHIFT -> value = new LeftShift(value, secondPriority(parts));
                case RIGHT_SHIFT -> value = new RightShift(value, secondPriority(parts));
                case ARITHMETICAL_RIGHT_SHIFT -> value = new RightArithmShift(value, secondPriority(parts));
                case EOF, RIGHT -> {
                    parts.back();
                    return value;
                }
                default -> throw new IllegalArgumentException();
            }
        }
    }

    public static Arithmetical secondPriority(StringPartBuffer parts) {
        Arithmetical value = firstPriority(parts);
        while (true) {
            StringPart part = parts.next();
            switch (part.type) {
                case PlUS -> value = new Add(value, firstPriority(parts));
                case MINUS -> value = new Subtract(value, firstPriority(parts));
                case EOF, RIGHT, RIGHT_SHIFT, LEFT_SHIFT, ARITHMETICAL_RIGHT_SHIFT -> {
                    parts.back();
                    return value;
                }
                default -> throw new IllegalArgumentException();
            }
        }
    }

    public static Arithmetical firstPriority(StringPartBuffer parts) {
        Arithmetical value = zeroPriority(parts);
        while (true) {
            StringPart part = parts.next();
            switch (part.type) {
                case MULTIPLE -> value = new Multiply(value, zeroPriority(parts));
                case DIVIDE -> value = new Divide(value, zeroPriority(parts));
                case EOF, RIGHT, PlUS, MINUS, RIGHT_SHIFT, LEFT_SHIFT, ARITHMETICAL_RIGHT_SHIFT -> {
                    parts.back();
                    return value;
                }
                default -> throw new IllegalArgumentException();
            }
        }
    }

    public static Arithmetical zeroPriority(StringPartBuffer parts) {
        StringPart part = parts.next();
        switch (part.type) {
            case RIGHT_ZERO:
                return new RightZero(zeroPriority(parts));
            case LEFT_ZERO:
                return new LeftZero(zeroPriority(parts));
            case MINUS:
                return new Negation(zeroPriority(parts));
            case CONST:
                return new Const(Integer.parseInt(part.value));
            case VARIABLE:
                return new Variable(part.value);
            case LEFT:
                Arithmetical value = thirdPriority(parts);
                part = parts.next();
                if (part.type != ArithmeticalExpressionType.RIGHT) {
                    throw new IllegalArgumentException();
                }
                return value;
            default:
                throw new IllegalArgumentException();
        }
    }
}

