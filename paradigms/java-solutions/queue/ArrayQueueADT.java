package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    private Object[] base = new Object[1];
    private int head = 0;
    private int size = 0;
    private int tail = 0;

    private static void ensureCapacity(ArrayQueueADT queue, int s) {
        if (s > queue.base.length) {
            Object[] copy = new Object[queue.base.length * 2];
            System.arraycopy(queue.base, queue.head, copy, 0, queue.base.length - queue.head);
            System.arraycopy(queue.base, 0, copy, queue.base.length - queue.head, queue.head);
            queue.base = copy;
            queue.tail = queue.size - 2;
            queue.head = 0;
        }
    }

    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;
        queue.size++;
        ensureCapacity(queue, queue.size);
        queue.tail = (queue.tail + 1) % queue.base.length;
        queue.base[queue.tail] = element;
    }

    public static Object element(ArrayQueueADT queue) {
        return queue.base[queue.head];
    }

    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.head < queue.base.length;
        Object res = queue.base[queue.head];
        queue.base[queue.head] = null;
        queue.head++;
        if (queue.head == queue.base.length) {
            queue.head = 0;
        }
        queue.size--;
        return res;
    }

    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    public static void clear(ArrayQueueADT queue) {
        queue.head = 0;
        queue.tail = 0;
        queue.size = 0;
        queue.base = new Object[1];
    }

    public static int indexOf(ArrayQueueADT queue, Object forSearch) {
        for (int i = queue.head; i < queue.base.length; i++) {
            if (queue.base[i] != null && queue.base[i].equals(forSearch)) {
                return i - queue.head;
            }
        }
        for (int i = 0; i <= queue.tail; i++) {
            if (queue.base[i] != null && queue.base[i].equals(forSearch)) {
                return i + (queue.base.length) - queue.head;
            }
        }
        return -1;
    }

    public static int lastIndexOf(ArrayQueueADT queue, Object forSearch) {
        for (int i = queue.tail; i >= 0; i--) {
            if (queue.base[i] != null && queue.base[i].equals(forSearch)) {
                return queue.size - 1 - (queue.tail - i);
            }
        }
        for (int i = queue.base.length - 1; i >= queue.head; i--) {
            if (queue.base[i] != null && queue.base[i].equals(forSearch)) {
                return (i - queue.head);
            }
        }
        return -1;
    }
}
