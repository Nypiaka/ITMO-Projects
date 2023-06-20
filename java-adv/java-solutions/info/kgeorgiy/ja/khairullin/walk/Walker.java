package info.kgeorgiy.ja.khairullin.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

public class Walker extends SimpleFileVisitor<Path> {
    private final BufferedWriter writer;

    public Walker(BufferedWriter writer) {
        this.writer = writer;
    }

    public void writeByFormat(String hashCode, String filename) throws IOException {
        writer.write(hashCode + " " + filename + System.lineSeparator());
    }

    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        writeByFormat(HashCalculator.calculateHash(file.toString()), file.toString());
        return super.visitFile(file, attrs);
    }

    @Override
    public FileVisitResult visitFileFailed(Path filePath, IOException e) throws IOException {
        System.err.println(e.getMessage());
        writeByFormat(HashCalculator.ERROR_HASH, filePath.toString());
        return FileVisitResult.CONTINUE;
    }

}