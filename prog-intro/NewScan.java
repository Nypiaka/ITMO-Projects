import java.io.*;

public class NewScan implements AutoCloseable {
    private final int bufferSize = 256;
    private Reader reader;
    private char[] buffer = new char[bufferSize];
    private int counter = 0;
    private String s0;
    private int len = 0;
    public String lettersOfCurrentString;
    private int pos = 0;
    private int i = 0;
    private int prev = 0;
    private int type = 0; //0-word, 1-int, 2-next, 3-line, 4-hex/abc/count
    private boolean EOF = false;

    public NewScan(InputStream inputstream) throws IOException {
        this.reader = new InputStreamReader(System.in);
    }

    public NewScan(String inputstream) throws IOException {
        this.reader = new StringReader(inputstream);
    }

    public NewScan(File f, String charSet) throws IOException {
        this.reader = new InputStreamReader(new FileInputStream(f), charSet);
    }

    private void readBuffer() throws IOException {
        len = reader.read(buffer);
        if (len < bufferSize && len > 0) {
            buffer[len] = '\n';
        }
        pos = 0;
        counter++;
        if (len == -1) {
            EOF = true;
        }
    }

    private boolean charChecker(char e) {
        if (type == 0) {
            return Character.isLetter(e) || Character.getType(e) == Character.DASH_PUNCTUATION || e == '\'';
        } else if (type == 1) {
            return e == '-' || (Character.getNumericValue(e) <= 9 && Character.getNumericValue(e) >= 0);
        } else if (type == 4) {
            return e == '-' || (Character.getNumericValue(e) <= 9 && Character.getNumericValue(e) >= 0) || e == 'x' || e == 'X' || Character.isLetter(e);
        } else if (type == 2) {
            return e != ' ' && e != '\n';
        } else {
            return e != '\n';
        }
    }

    private boolean nextCharByType(int tip) throws IOException {
        if (counter == 0) {
            readBuffer();
        }
        type = tip;
        while (!charChecker(buffer[pos]) && pos < len) {
            pos++;
            if (pos == len && !EOF) {
                readBuffer();
            }
        }
        return !EOF;
    }

    public String nextSmth(int tip) throws IOException {
        if (counter == 0) {
            readBuffer();
        }
        StringBuilder se = new StringBuilder();
        type = tip;
        {
            if (type != 3) {
                while (!charChecker(buffer[pos]) && pos < len) {
                    pos++;
                    if (pos == len && !EOF) {
                        readBuffer();
                    }
                }
                while (charChecker(buffer[pos]) && pos < len) {
                    se.append(buffer[pos]);
                    pos++;
                    if (pos == len && !EOF) {
                        readBuffer();
                    }
                }
                return se.toString();
            } else {
                if (buffer[pos] == '\n') {
                    if (pos == len - 1) {
                        readBuffer();
                    } else pos++;
                    return "";
                } else {
                    while (charChecker(buffer[pos]) && pos < len) {
                        se.append(buffer[pos]);
                        pos++;
                        if (pos == len && !EOF) {
                            readBuffer();
                        }
                    }
                    if (pos == len - 1) {
                        readBuffer();
                    } else pos++;
                    return se.toString();
                }
            }
        }
    }

    public int nextInt() throws IOException {
        String s = nextSmth(1);
        try {
            int value = Integer.parseInt(s);
            return value;
        } catch (NumberFormatException e) {
            throw new IOException("No int to read");
        }
    }

    public String nextHex() throws IOException {
        String s = nextSmth(4);
        if (s != null) {
            return s;
        } else throw new IOException("No hex or abc or count to read");
    }

    public String nextWord() throws IOException {
        String s = nextSmth(0);
        if (s != null) {
            return s;
        } else throw new IOException("No word to read");
    }

    public String next() throws IOException {
        String s = nextSmth(2);
        if (s != null) {
            return s;
        } else throw new IOException("No word to read");
    }

    public String nextLine() throws IOException {
        String s = nextSmth(3);
        return s;
    }

    public boolean hasNextInt() throws IOException {
        return nextCharByType(1);
    }

    public boolean hasNext() throws IOException {
        return nextCharByType(2);
    }

    public boolean hasNextWord() throws IOException {
        return nextCharByType(0);
    }

    public boolean hasNextLine() throws IOException {
        return !EOF;
    }

    public void close() throws Exception {
        reader.close();
    }
}
