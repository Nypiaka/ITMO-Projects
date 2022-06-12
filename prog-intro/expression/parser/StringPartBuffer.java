package expression.parser;

import java.util.List;

public class StringPartBuffer {
    private int pos = 0;

    public List<StringPart> base;

    public StringPartBuffer(List<StringPart> list) {
        this.base = list;
    }

    public StringPart next() {
        return base.get(pos++);
    }

    public void back() {
        pos--;
    }

    public int getPos() {
        return pos;
    }
}
