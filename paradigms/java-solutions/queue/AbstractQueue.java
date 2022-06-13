
package queue;

import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size;

    public boolean isEmpty() {
        return this.size == 0;
    }

    public int size() {
        return this.size;
    }

    abstract void nextVal();

    abstract Object getVal();

    public int indexIf(Predicate a) {
        int counter = 0;
        while (this.notNull()) {
            if (a.test(this.getVal())) {
                this.re();
                return counter;
            }
            counter++;
            this.nextVal();
        }
        this.re();
        return -1;
    }

    public int lastIndexIf(Predicate a) {
        int counter = 0;
        int pos = -1;
        while (this.notNull()) {
            if (a.test(this.getVal())) {
                pos = counter;
            }
            counter++;
            this.nextVal();
        }
        this.re();
        return pos;
    }
}
