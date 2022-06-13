package expression.exceptions;

import expression.ArithmeticalExpressionType;

public class StringPart {
    ArithmeticalExpressionType type;
    String value;

    public StringPart(ArithmeticalExpressionType type, String value){
        this.type=type;
        this.value=value;
    }
}
