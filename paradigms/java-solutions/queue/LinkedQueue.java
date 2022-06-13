//
// Source code recreated from a .class file by IntelliJ IDEA
// (powered by FernFlower decompiler)
//

package queue;

import java.util.function.Predicate;

public class LinkedQueue extends AbstractQueue {
    private Link tail = new Link(null);
    private Link head;
    private Link iter;
    private boolean flag = true;

    {
        this.head = new Link(this.tail);
        iter = this.head;
    }

    public void enqueue(Object element) {
        if (this.size == 0) {
            this.head.value = element;
        } else if (this.size == 1) {
            this.tail.value = element;
        } else {
            this.tail.next = new Link(null);
            this.tail.next.value = element;
            this.tail = this.tail.next;
        }
        this.size++;
    }

    public Object element() {
        return this.head.value;
    }

    public Object dequeue() {
        Object val = this.head.value;
        if (size > 2) {
            head = head.next;
            iter = head;
        } else if (this.size == 2) {
            head.value = tail.value;
            iter.value = head.value;
            tail.value = null;
        } else {
            head.value = 0;
        }

        super.size--;
        return val;
    }

    public void clear() {
        tail = new Link(null);
        head = new Link(this.tail);
        iter = head;
        super.size = 0;
    }

    public int indexOf(Object forSearch) {
        int counter = 0;
        Link current = head;
        while (current != null) {
            if (forSearch.equals(current.value)) {
                System.out.println();
                System.out.println();
                System.out.println();
                return counter;
            }
            counter++;
            current = current.next;
        }
        System.out.println();
        System.out.println();
        System.out.println();
        return -1;
    }

    public int lastIndexOf(Object forSearch) {
        int counter = 0;
        Link current = head;
        int pos = -1;
        while (current != null) {
            if (forSearch.equals(current.value)) {
                pos = counter;
            }
            counter++;
            current = current.next;
        }
        System.out.println();
        System.out.println();
        System.out.println();
        return pos;
    }

    @Override
    public Object getVal() {
        return iter.value;
    }

    @Override
    public void nextVal() {
        if (flag) {
            iter = head.next;
            flag = false;
        } else {
            iter = iter.next;
        }
    }

    public static class Link {
        private Object value;
        private Link next;

        public Link(Link n) {
            this.next = n;
        }
    }

    @Override
    public void re() {
        iter = head;
    }

    @Override
    public boolean notNull() {
        return iter != null;
    }
}
