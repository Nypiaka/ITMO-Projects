package info.kgeorgiy.ja.khairullin.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * A class that returns the maximum or minimum of a list of elements,
 * and also checks that some or all elements of the list satisfy a predicate.
 * It can also count the number of elements that satisfy the predicate.
 * Class uses multithreading
 * <p>
 * Implementing {@link ScalarIP}
 *
 * @author Nypiaka
 */
public class IterativeParallelism implements ScalarIP {
    private final ParallelMapper parallelMapper;

    /**
     * Constructor of {@link IterativeParallelism};
     *
     * @param pm creates this ParallelMapper of {@link ParallelMapper};
     */
    public IterativeParallelism(ParallelMapper pm) {
        this.parallelMapper = pm;
    }

    /**
     * Constructor of {@link IterativeParallelism};
     * <p>
     * parallelMapper will be null
     */
    public IterativeParallelism() {
        this.parallelMapper = null;
    }

    private static class ScalarThread<T, R> extends Thread {
        private final List<? extends T> list;
        private final Function<Stream<? extends T>, R> currentMethod;
        private R threadWorkingResult;

        public ScalarThread(List<? extends T> values, Function<Stream<? extends T>, R> currentMethod) {
            this.list = values;
            this.currentMethod = currentMethod;
        }

        @Override
        public void run() {
            threadWorkingResult = currentMethod.apply(list.stream());
        }

        public R getThreadWorkingResult() {
            return threadWorkingResult;
        }
    }

    private <T, R> Stream<ScalarThread<T, R>> startAndStopThreads(int numOfThreads, List<? extends T> values,
                                                                  Function<Stream<? extends T>, R> currentMethod) throws InterruptedException {
        List<ScalarThread<T, R>> threads = createThreads(numOfThreads, values, currentMethod);
        threads.forEach(Thread::start);
        InterruptedException commonException = new InterruptedException();
        for (int i = 0; i < threads.size(); i++) {
            while (true) {
                try {
                    threads.get(i).join();
                    break;
                } catch (InterruptedException e) {
                    for (int j = i; i < threads.size(); j++) {
                        threads.get(j).interrupt();
                    }
                    commonException.addSuppressed(e);
                }
            }
        }
        if (commonException.getSuppressed().length != 0) {
            throw commonException;
        }
        return threads.stream();
    }

    private <T> void validate(int numOfThreads, List<? extends T> values) throws IllegalArgumentException {
        if (numOfThreads <= 0) {
            throw new IllegalArgumentException("num of threads should be more than 0");
        }
        if (values == null || values.size() == 0) {
            throw new IllegalArgumentException("values shouldn't be empty");
        }
    }

    private <T, R> List<ScalarThread<T, R>> createThreads(int numOfThreads, List<? extends T> values, Function<Stream<? extends T>, R> currentMethod) throws InterruptedException {
        validate(numOfThreads, values);
        ArrayList<ScalarThread<T, R>> threadsList = new ArrayList<>();
        List<List<? extends T>> tasks = createTasks(numOfThreads, values);
        for (List<? extends T> task : tasks) {
            threadsList.add(new ScalarThread<>(task, currentMethod));
        }
        return threadsList;
    }

    private <T, R> R calculate(Function<Stream<? extends T>, R> currentFunction,
                               Function<Stream<R>, R> functionToTransform,
                               int threads,
                               List<? extends T> values) throws InterruptedException {
        if (parallelMapper == null)
            return functionToTransform.apply(startAndStopThreads(threads, values, currentFunction).map(ScalarThread::getThreadWorkingResult));
        return functionToTransform.apply(parallelMapper.map(list -> currentFunction.apply(list.stream()), createTasks(threads, values)).stream());
    }

    private <T> List<List<? extends T>> createTasks(int threads, List<? extends T> values) {
        int numOfValues = values.size();
        int realNumOfThreads = Math.min(threads, numOfValues);
        int loadPerThread = numOfValues / realNumOfThreads;
        int reminder = numOfValues % realNumOfThreads;
        int previous = 0;
        List<List<? extends T>> parts = new ArrayList<>();
        for (int i = 0; i < realNumOfThreads; i++) {
            int reminderPart = reminder > 0 ? 1 : 0;
            parts.add(values.subList(previous, previous + loadPerThread + reminderPart));
            previous += loadPerThread + reminderPart;
            reminder -= reminderPart;
        }
        return parts;
    }

    /**
     * Returns maximum value.
     *
     * @param threads    number of concurrent threads.
     * @param values     values to get maximum of.
     * @param comparator value comparator.
     * @param <T>        value type.
     * @return maximum of given values
     * @throws InterruptedException             if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return calculate(stream -> stream.max(comparator).orElseThrow(), stream -> stream.max(comparator).orElseThrow(), threads, values);
    }

    /**
     * Returns minimum value.
     *
     * @param threads    number of concurrent threads.
     * @param values     values to get minimum of.
     * @param comparator value comparator.
     * @param <T>        value type.
     * @return minimum of given values
     * @throws InterruptedException             if executing thread was interrupted.
     * @throws java.util.NoSuchElementException if no values are given.
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, comparator.reversed());
    }

    /**
     * Returns whether all values satisfy predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     * @return whether all values satisfy predicate or {@code true}, if no values are given.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    /**
     * Returns whether any of values satisfies predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     * @return whether any value satisfies predicate or {@code false}, if no values are given.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return calculate(stream -> stream.anyMatch(predicate), stream -> stream.anyMatch(t -> t), threads, values);
    }

    /**
     * Returns number of values satisfying predicate.
     *
     * @param threads   number of concurrent threads.
     * @param values    values to test.
     * @param predicate test predicate.
     * @param <T>       value type.
     * @return number of values satisfying predicate.
     * @throws InterruptedException if executing thread was interrupted.
     */
    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return calculate(stream -> Math.toIntExact(stream.filter(predicate).count()),
                threadsStream -> threadsStream.flatMapToInt(IntStream::of).sum(), threads, values);
    }
}