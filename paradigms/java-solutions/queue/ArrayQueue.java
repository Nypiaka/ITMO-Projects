package queue;

import java.util.Arrays;
import java.util.function.Predicate;

public class ArrayQueue extends AbstractQueue {
    private Object[] base = new Object[1];
    private int head = 0;
    private int tail = 0;
    private int iter = 0;
    private boolean flag = true;

    private void ensureCapacity(int s) {
        if (s > base.length) {
            Object[] copy = new Object[base.length * 2];
            System.arraycopy(base, head, copy, 0, base.length - head);
            System.arraycopy(base, 0, copy, base.length - head, head);
            base = copy;
            tail = super.size - 2;
            head = 0;
        }
        iter = head;
    }

    public void enqueue(Object element) {
        super.size++;
        ensureCapacity(super.size);
        tail = (tail + 1) % base.length;
        base[tail] = element;
        iter = head;
    }

    public Object element() {
        return base[head];
    }

    public Object dequeue() {
        Object res = base[head];
        base[head] = null;
        head++;
        super.size--;
        if (head == base.length) {
            head = 0;
        }
        iter = head;
        return res;
    }

    public void clear() {
        head = 0;
        iter = 0;
        tail = 0;
        super.size = 0;
        base = new Object[1];
    }

    public int indexOf(Object forSearch) {
        for (int i = head; i < base.length; i++) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return i - head;
            }
        }
        for (int i = 0; i <= tail; i++) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return i + (base.length) - head;
            }
        }
        return -1;
    }

    public int lastIndexOf(Object forSearch) {
        for (int i = tail; i >= 0; i--) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return super.size - 1 - (tail - i);
            }
        }
        for (int i = base.length - 1; i >= head; i--) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return (i - head);
            }
        }
        return -1;
    }

    @Override
    public void nextVal() {
        iter++;
        if (iter == base.length && base.length != tail + 1) {
            iter = 0;
        }
    }

    @Override
    public Object getVal() {
        return base[iter];
    }

    @Override
    public void re() {
        iter = head;
        flag = true;
    }

    @Override
    public boolean notNull() {
        if (flag && base[head] != null) {
            flag = false;
            return true;
        }

        return iter != tail + 1;
    }

}
