package queue;

import java.util.Arrays;

public class ArrayQueueModule {
    private static Object[] base = new Object[1];
    private static int head = 0;
    private static int size = 0;
    private static int tail = 0;

    private static void ensureCapacity(int s) {
        if (s > base.length) {
            Object[] copy = new Object[base.length * 2];
            System.arraycopy(base, head, copy, 0, base.length - head);
            System.arraycopy(base, 0, copy, base.length - head, head);
            base = copy;
            tail = size - 2;
            head = 0;
        }
    }

    public static void enqueue(Object element) {
        assert element != null;
        size++;
        ensureCapacity(size);
        tail = (tail + 1) % base.length;
        base[tail] = element;

    }

    public static Object element() {
        return base[head];
    }

    public static Object dequeue() {
        Object res = base[head];
        base[head] = null;
        head++;
        size--;
        if (head == base.length) {
            head = 0;
        }
        return res;
    }

    public static int size() {
        return size;
    }

    public static boolean isEmpty() {
        return size == 0;
    }

    public static void clear() {
        head = 0;
        tail = 0;
        size = 0;
        base = new Object[1];
    }

    public static int indexOf(Object forSearch) {
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

    public static int lastIndexOf(Object forSearch) {
        for (int i = tail; i >= 0; i--) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return size - 1 - (tail - i);
            }
        }
        for (int i = base.length - 1; i >= head; i--) {
            if (base[i] != null && base[i].equals(forSearch)) {
                return (i - head);
            }
        }
        return -1;
    }
}
