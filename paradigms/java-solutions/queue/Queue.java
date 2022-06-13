
package queue;

import java.util.function.Predicate;

public interface Queue {
    // Model
    // n -- size queue
    // a[1..n] -- queue
    private void ensureCapacity() {
    }
    //Pred: element != null
    //Post: a[n] = element; n++;
    void enqueue(Object var1);
    //Pred: n >= 1
    //Post: return a[1]; immutable
    Object element();
    //Pred: n>=1
    //Post: R = a[1], a = [a[2], .. a[n]], n--;
    Object dequeue();
    //Pred: true
    //Post: R = n, immutable
    int size();
    //Pred: true
    //Post: R = (n == 0), immutable
    boolean isEmpty();
    //Pred: true
    //Post: n = 0; a[] = [null]
    void clear();
    //Pred: n > 0, forSearch != null
    //Post: if a[] has forSearch, returns i: a[i] equals forSearch and i min, else return -1, immutable
    int indexOf(Object var1);
    //Pred: n > 0, forSearch != null
    //Post: if a has forSearch, returns i: a[i] equals forSearch and i max, else return -1, immutable
    int lastIndexOf(Object var1);
    //Pred: true
    //Post: if a has R: Predicate.test(a): returns i: Predicate.test(a[i]) and i min, else return -1, immutable
    int indexIf(Predicate var1);
    //Pred: true
    //Post: if a has R: Predicate.test(a): returns i: Predicate.test(a[i]) and i max, else return -1, immutable
    int lastIndexIf(Predicate var1);

    void re();

    boolean notNull();
}
