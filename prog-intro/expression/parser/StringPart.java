package expression.parser;

public class StringPart {
    ArithmeticalExpressionType type;
    String value;

    public StringPart(ArithmeticalExpressionType type, String value){
        this.type=type;
        this.value=value;
    }
}
