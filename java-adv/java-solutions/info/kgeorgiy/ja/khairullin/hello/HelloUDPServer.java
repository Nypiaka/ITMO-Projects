package info.kgeorgiy.ja.khairullin.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.*;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

public class HelloUDPServer implements HelloServer {
    private ExecutorService executorService;
    private DatagramSocket socket;

    private final int TIME_OUT = 500;

    private boolean SHOULD_PRINT_STACKTRACE = false;

    private final static String ERROR_MESSAGE = "expected type of input parameters: port (int), threads (int)";

    public static void main(String[] args) {
        if (args == null || args.length < 2 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException(ERROR_MESSAGE);
        }
        try (HelloServer server = new HelloUDPServer()) {
            server.start(Integer.parseInt(args[0]), Integer.parseInt(args[1]));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(ERROR_MESSAGE);
        }
    }

    @Override
    public void start(int port, int threads) {
        executorService = Executors.newFixedThreadPool(threads);
        try {
            socket = new DatagramSocket(port);
        } catch (SocketException e) {
            e.printStackTrace();
            return;
        }
        DatagramSocket finalSocket = socket;
        IntStream.range(0, threads).forEach(i -> executorService.submit(() -> {
                    while (true) {
                        try {
                            byte[] buffer = new byte[socket.getReceiveBufferSize()];
                            DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
                            finalSocket.receive(packet);
                            String response = "Hello, " + new String(packet.getData(), 0, packet.getLength());
                            DatagramPacket dp = new DatagramPacket(
                                    response.getBytes(),
                                    response.getBytes().length,
                                    packet.getAddress(),
                                    packet.getPort());
                            finalSocket.send(dp);
                        } catch (IOException e) {
                            if (SHOULD_PRINT_STACKTRACE) {
                                e.printStackTrace();
                            }
                            close();
                            return;
                        }
                    }
                }
        ));
    }

    @Override
    public void close() {
        if (socket != null && executorService != null) {
            do {
                socket.close();
            } while (!socket.isClosed());
            try {
                if (!executorService.awaitTermination(TIME_OUT, TimeUnit.MILLISECONDS)) {
                    executorService.shutdown();
                }
            } catch (InterruptedException e) {
                executorService.shutdown();
            }
        }
    }
}