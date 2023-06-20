package info.kgeorgiy.ja.khairullin.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements SortedSet<E> {
    private final List<E> data;
    private final Comparator<? super E> comparator;

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        TreeSet<E> tmpSet = new TreeSet<>(comparator);
        tmpSet.addAll(collection);
        this.data = tmpSet.stream().toList();
        this.comparator = comparator;
    }

    @SuppressWarnings("unchecked")
    public boolean contains(final Object element) {
        return Collections.binarySearch(this.data, (E) element, this.comparator) >= 0;
    }

    @Override
    public Comparator<? super E> comparator() {
        return this.comparator;
    }

    private int find(E element) {
        int result = Collections.binarySearch(this.data, element, this.comparator);
        return result >= 0 ? result : -(result + 1);
    }

    void checkNotEmpty() {
        if (this.data.size() == 0) {
            throw new NoSuchElementException();
        }
    }

    private SortedSet<E> subSetByNums(int from, int to) {
        return new ArraySet<>(this.data.subList(from, to), this.comparator);
    }

    @SuppressWarnings("unchecked")
    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        if ((this.comparator != null && Objects.compare(fromElement, toElement, this.comparator) > 0) ||
                (!Objects.nonNull(this.comparator) && ((Comparable<E>) fromElement).compareTo(toElement) > 0)) {
            throw new IllegalArgumentException();
        }
        return subSetByNums(find(fromElement), find(toElement));
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return subSetByNums(0, find(toElement));
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return subSetByNums(find(fromElement), this.data.size());
    }

    @Override
    public E first() {
        checkNotEmpty();
        return this.data.get(0);
    }

    @Override
    public E last() {
        checkNotEmpty();
        return this.data.get(this.data.size() - 1);
    }

    @Override
    public int size() {
        return this.data.size();
    }

    @Override
    public boolean isEmpty() {
        return this.data.size() == 0;
    }

    @Override
    public Iterator<E> iterator() {
        return this.data.iterator();
    }

}

