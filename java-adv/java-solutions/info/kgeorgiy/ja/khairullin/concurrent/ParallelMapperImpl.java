package info.kgeorgiy.ja.khairullin.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.stream.IntStream;

/**
 * A class that thread-safely executes a function on an array of arguments and returns an array of results.
 * <p>
 * Implementing {@link ParallelMapper}
 *
 * @author Nypiaka
 */
public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threads;
    private final ArrayDeque<Runnable> tasks;

    /**
     * Constructor of {@link ParallelMapperImpl};
     *
     * @param threads creates this count of {@link Thread threads};
     */
    public ParallelMapperImpl(final int threads) throws IllegalAccessException {
        if (threads <= 0) throw new IllegalAccessException();
        this.tasks = new ArrayDeque<>();
        this.threads = new ArrayList<>();
        IntStream.range(0, threads).forEach(i -> {
            this.threads.add(new Thread(() -> {
                try {
                    while (!Thread.interrupted()) {
                        Runnable task;
                        synchronized (tasks) {
                            while (tasks.isEmpty()) {
                                tasks.wait();
                            }
                            task = tasks.poll();
                            tasks.notify();
                        }
                        task.run();
                    }
                } catch (InterruptedException ignored) {
                } finally {
                    Thread.currentThread().interrupt();
                }
            }));
            this.threads.get(i).start();
        });
    }

    /**
     * Maps function {@code f} over specified {@code args}.
     * Mapping for each element performed in parallel.
     *
     * @throws InterruptedException if calling thread was interrupted
     */
    @Override
    public <T, R> List<R> map(Function<? super T, ? extends R> f, List<? extends T> args) throws InterruptedException {
        final List<R> res = new ArrayList<>(Collections.nCopies(args.size(), null));
        AtomicCounter counter = new AtomicCounter(args.size());
        RuntimeException[] exceptions = {null};
        IntStream.range(0, args.size()).forEach(i -> {
                    synchronized (tasks) {
                        tasks.add(() -> {
                            try {
                                res.set(i, f.apply(args.get(i)));
                            } catch (RuntimeException e) {
                                synchronized (exceptions) {
                                    if (exceptions[0] == null) {
                                        exceptions[0] = e;
                                    } else exceptions[0].addSuppressed(e);
                                }
                            }
                            counter.increment();
                        });
                        tasks.notify();
                    }
                }
        );
        if (exceptions[0] != null) {
            throw exceptions[0];
        }
        counter.waitToCount();
        return res;
    }

    /**
     * Stops all threads. All unfinished mappings are left in undefined state.
     */
    @Override
    public void close() {
        threads.forEach(Thread::interrupt);
        threads.forEach(t -> {
            while (true) {
                try {
                    t.join();
                    break;
                } catch (InterruptedException ignored) {
                }
            }
        });
    }

    private static class AtomicCounter {
        private int counter;
        private final int finalValue;

        private AtomicCounter(int finalValue) {
            this.counter = 0;
            this.finalValue = finalValue;
        }

        private synchronized void increment() {
            counter++;
            notify();
        }

        private synchronized void waitToCount() throws InterruptedException {
            while (counter != finalValue) {
                wait();
            }
        }
    }
}