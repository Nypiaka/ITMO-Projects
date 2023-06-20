package info.kgeorgiy.ja.khairullin.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.IntStream;

public class HelloUDPClient implements HelloClient {

    private final int TIME_OUT = 200;
    private static final String ERROR_MESSAGE = "expected type of input parameters: host (string), port (int), prefix (string), threads (int), requests (int)";

    public static void main(String[] args) {
        if (args == null || args.length < 5 || Arrays.stream(args).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException(ERROR_MESSAGE);
        }
        try {
            new HelloUDPClient().run(args[0], Integer.parseInt(args[1]), args[2], Integer.parseInt(args[3]), Integer.parseInt(args[4]));
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(ERROR_MESSAGE);
        }
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try (ExecutorService executor = Executors.newFixedThreadPool(threads)) {
            IntStream.range(1, threads + 1).forEach(tmpThread -> {
                executor.submit(() -> {
                    try (DatagramSocket currentThreadSocket = new DatagramSocket()) {
                        currentThreadSocket.setSoTimeout(TIME_OUT);
                        DatagramPacket reply = new DatagramPacket(new byte[currentThreadSocket.getReceiveBufferSize()], currentThreadSocket.getReceiveBufferSize());
                        IntStream.range(1, requests + 1).forEach(numOfRequest -> {
                            String request = prefix + tmpThread + "_" + numOfRequest;
                            String response = "";
                            waitCorrectResponse(currentThreadSocket, createNewDatagramPacketToSend(request, host, port), reply, response, request);
                        });
                    } catch (SocketException e) {
                        e.printStackTrace();
                    }
                });
            });
        }
    }

    private void waitCorrectResponse(DatagramSocket currentThreadSocket, DatagramPacket datagramPacket, DatagramPacket reply, String response, String request) {
        do {
            try {
                currentThreadSocket.send(datagramPacket);
                currentThreadSocket.receive(reply);
                byte[] data = reply.getData();
                response = new String(data, reply.getOffset(), reply.getLength());
            } catch (SocketTimeoutException ignored) {
            } catch (IOException e) {
                e.printStackTrace();
            }
        } while (!response.equals("Hello, " + request) && !currentThreadSocket.isClosed());
        System.out.println("success response");
    }

    private DatagramPacket createNewDatagramPacketToSend(String request, String host, int port) {
        DatagramPacket datagramPacket = null;
        try {
            datagramPacket = new DatagramPacket(request.getBytes(StandardCharsets.UTF_8), request.getBytes(StandardCharsets.UTF_8).length, InetAddress.getByName(host), port);
        } catch (UnknownHostException e) {
            e.printStackTrace();
        }
        return datagramPacket;
    }

}