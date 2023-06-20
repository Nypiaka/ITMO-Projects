package info.kgeorgiy.ja.khairullin.walk;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class HashCalculator {
    public static final String ERROR_HASH = "0".repeat(64);

    public static String calculateHash(String fileName) {
        try (InputStream bufferedInputStream = Files.newInputStream(Path.of(fileName))) {
            byte[] buffer = new byte[512];
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            int count;
            while ((count = bufferedInputStream.read(buffer)) > 0) {
                digest.update(buffer, 0, count);
            }

            bufferedInputStream.close();
            byte[] hash = digest.digest();
            StringBuilder sb = new StringBuilder();
            for (byte b : hash) {
                sb.append(Integer.toString((b & 0xff) + 0x100, 16).substring(1));
            }
            return sb.toString();
        } catch (IOException | NoSuchAlgorithmException | InvalidPathException e) {
            return ERROR_HASH;
        }
    }
}