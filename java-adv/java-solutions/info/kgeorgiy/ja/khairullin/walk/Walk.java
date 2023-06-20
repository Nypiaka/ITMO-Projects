package info.kgeorgiy.ja.khairullin.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;

public class Walk {

    public static void main(String[] args) {
        if (args == null) {
            System.err.println("Wrong input format. No input names.");
            return;
        }
        if (args.length <= 1) {
            System.err.println("No output file name");
            return;
        }
        if (args[0] == null) {
            System.err.println("No input file name");
            return;
        }
        if (args[1] == null) {
            System.err.println("No output file name");
            return;
        }
        try {
            if (!Files.exists(Path.of(args[1]))) {
                if (Path.of(args[1]).getParent() != null) {
                    try {
                        Files.createDirectories(Path.of(args[1]).getParent());
                    } catch (IOException e) {
                        System.err.println("Can't create directory");
                    }
                } else return;
            }
        } catch (InvalidPathException e) {
            System.out.println(e.getMessage());
            return;
        }
        try (BufferedReader reader = Files.newBufferedReader(Path.of(args[0]), StandardCharsets.UTF_8)) {
            try (BufferedWriter writer = Files.newBufferedWriter(Path.of(args[1]), StandardCharsets.UTF_8)) {
                String currentFileName;
                while ((currentFileName = reader.readLine()) != null) {
                    writer.write(HashCalculator.calculateHash(currentFileName).toLowerCase() + " " + currentFileName + System.lineSeparator());
                }
            } catch (IOException e) {
                System.err.println("Input files problem" + System.lineSeparator() + e.getMessage());
            }
        } catch (IOException e) {
            System.err.println("Output files problem" + System.lineSeparator() + e.getMessage());
        }
    }
}