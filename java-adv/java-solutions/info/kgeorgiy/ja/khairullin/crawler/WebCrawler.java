package info.kgeorgiy.ja.khairullin.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.IntStream;

/**
 * A class that traverses web pages recursively. The bypass implementation uses multithreading.
 *
 * <p>
 * Implements {@link Crawler}
 *
 * @author Nypiaka
 */
public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final ExecutorService downloadingExecutor;
    private final ExecutorService extractingExecutor;


    /**
     * Constructor of {@link WebCrawler};
     *
     * @param downloader  is url pages {@link Downloader};
     * @param downloaders is max number of downloading threads;
     * @param extractors  is max number of extracting threads;
     * @param perHost     is max num of downloading pages per host (not using in my realisation);
     */
    public WebCrawler(Downloader downloader,
                      int downloaders,
                      int extractors,
                      int perHost) {
        this.downloader = downloader;
        this.downloadingExecutor = Executors.newFixedThreadPool(downloaders);
        this.extractingExecutor = Executors.newFixedThreadPool(extractors);
    }

    private static void throwArgsException() {
        String argsErrorMessage = "Wrong args presentation. Args should be: url [depth [downloads [extractors [perHost]]]], where url is string, other are positive int";
        throw new IllegalArgumentException(argsErrorMessage);
    }


    /**
     * Starts crawling from command line
     *
     * @param args - arguments for downloading in format url [depth [downloads [extractors [perHost]]]]
     */
    public static void main(String[] args) {
        if (args == null || args.length == 0) {
            throwArgsException();
        }
        int[] defaultArgs = new int[]{1, 1, 1, 1};
        int[] resultArgs = new int[4];
        for (int i = 1; i < 5; i++) {
            if (args.length > i) {
                try {
                    resultArgs[i - 1] = Integer.parseInt(args[i]);
                    if (resultArgs[i - 1] < 0) {
                        throwArgsException();
                    }
                } catch (NumberFormatException e) {
                    throwArgsException();
                }
            } else {
                resultArgs[i - 1] = defaultArgs[i - 1];
            }
        }
        try (WebCrawler crawler = new WebCrawler(new CachingDownloader(10), resultArgs[1], resultArgs[2], resultArgs[3])) {
            crawler.download(args[0], resultArgs[0]);
        } catch (IOException e) {
            throw new IllegalStateException("Problems with downloader creation. Reason: " + Arrays.toString(e.getStackTrace()));
        }
    }

    private Future<Document> downloadParallel(String finalUrl,
                                              Set<String> visitedSuccess,
                                              Map<String, IOException> errors,
                                              Phaser phaser) {
        return downloadingExecutor.submit(() -> {
            try {
                return downloader.download(finalUrl);
            } catch (IOException e) {
                visitedSuccess.remove(finalUrl);
                errors.put(finalUrl, e);
                return null;
            } finally {
                phaser.arriveAndDeregister();
            }
        });
    }

    /**
     * Downloads website up to specified depth.
     *
     * @param url   start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @return download result.
     */
    @Override
    public Result download(String url, int depth) {
        Phaser phaser = new Phaser(1);
        Map<String, IOException> errors = new ConcurrentHashMap<>();
        Set<String> visitedSuccess = new ConcurrentSkipListSet<>();
        List<String> currentLevelQueue = new CopyOnWriteArrayList<>();
        List<String> nextLevelQueue = new CopyOnWriteArrayList<>();
        currentLevelQueue.add(url);
        visitedSuccess.add(url);
        IntStream.range(0, depth).forEach(level -> {
            nextLevelQueue.clear();
            boolean notFinalDepth = level != depth - 1;
            currentLevelQueue.forEach(finalUrl -> {
                phaser.register();
                Future<Document> downloaded = downloadParallel(finalUrl, visitedSuccess, errors, phaser);
                if (notFinalDepth) {
                    extractParallelAndUpdateQueue(phaser, downloaded, visitedSuccess, nextLevelQueue, errors);
                }
            });
            if (notFinalDepth) {
                currentLevelQueue.clear();
                phaser.arriveAndAwaitAdvance();
                currentLevelQueue.addAll(new HashSet<>(nextLevelQueue));
            }
        });
        phaser.arriveAndAwaitAdvance();
        return new Result(new ArrayList<>(visitedSuccess), errors);
    }

    private void extractParallelAndUpdateQueue(Phaser phaser,
                                               Future<Document> downloaded,
                                               Set<String> visited,
                                               List<String> newQueue,
                                               Map<String, IOException> errors) {
        phaser.register();
        extractingExecutor.submit(() -> {
            try {
                Document document = downloaded.get();
                if (document != null) {
                    document.extractLinks().stream()
                            .filter(tmpUrl -> !visited.contains(tmpUrl) && !errors.containsKey(tmpUrl))
                            .forEach(tmpUrl -> {
                                visited.add(tmpUrl);
                                newQueue.add(tmpUrl);
                            });
                }
            } catch (InterruptedException | ExecutionException | IOException ignored) {
            } finally {
                phaser.arriveAndDeregister();
            }
        });
    }

    /**
     * Closes this web-crawler, relinquishing any allocated resources.
     */
    @Override
    public void close() {
        while (!extractingExecutor.isShutdown()) {
            extractingExecutor.shutdown();
        }
        while (!downloadingExecutor.isShutdown()) {
            downloadingExecutor.shutdown();
        }
    }
}
