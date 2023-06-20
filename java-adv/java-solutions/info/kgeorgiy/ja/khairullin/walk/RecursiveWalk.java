package info.kgeorgiy.ja.khairullin.walk;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Objects;

public class RecursiveWalk {

    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.err.println("Wrong input format");
            return;
        }
        try (
                BufferedReader reader = new BufferedReader(new FileReader(args[0], StandardCharsets.UTF_8));
                BufferedWriter writer = new BufferedWriter(new FileWriter(args[1], StandardCharsets.UTF_8))
        ) {
            String currentFileName;
            Walker walker = new Walker(writer);
            while ((currentFileName = reader.readLine()) != null) {
                try {
                    Path currentPath = Path.of(currentFileName);
                    Files.walkFileTree(currentPath, walker);
                } catch (InvalidPathException e) {
                    walker.writeByFormat(HashCalculator.ERROR_HASH, currentFileName);
                }
            }
        } catch (IOException e) {
            System.err.println(e.getMessage());
        }
    }
}
