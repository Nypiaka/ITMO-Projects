package md2html;

import java.io.IOException;

public class Md2Html {
    public static void main(String[] args) throws IOException {
        try (NewParse parser = new NewParse(args[0], args[1], "utf-8")) {
            parser.parse();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}