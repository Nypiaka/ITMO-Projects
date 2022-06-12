package md2html;

import java.io.*;
import java.util.ArrayDeque;
import java.util.HashMap;

public class NewParse implements AutoCloseable {
    private final BufferedWriter writer;
    private final BufferedReader reader;
    private static final HashMap<String, Integer> taker = new HashMap<>();
    private static final HashMap<Integer, String> openTaker = new HashMap<>();
    private static final HashMap<Integer, String> closeTaker = new HashMap<>();
    private final HashMap<String, Integer> conditionChecker = new HashMap<>();
    private final HashMap<Integer, Integer> condition = new HashMap<>();
    private final ArrayDeque<StrInt> bas = new ArrayDeque<>();

    public NewParse(String inp, String output, String chrset) throws IOException {
        writer = new BufferedWriter(new OutputStreamWriter(new
                FileOutputStream(output), chrset));
        reader = new BufferedReader(new InputStreamReader(new FileInputStream(inp), chrset));
    }


    {
        {
            openTaker.put(4, "<strong>");
            openTaker.put(3, "<strong>");
            openTaker.put(1, "<em>");
            openTaker.put(2, "<em>");
            openTaker.put(9, "<s>");
            openTaker.put(8, "<code>");
            openTaker.put(5, "<ins>");
            openTaker.put(6, "<del>");
            openTaker.put(7, "<q>");
            openTaker.put(12, "<pre>");
            openTaker.put(13, "<var>");
        }
    }

    {
        {
            closeTaker.put(4, "</strong>");
            closeTaker.put(3, "</strong>");
            closeTaker.put(1, "</em>");
            closeTaker.put(2, "</em>");
            closeTaker.put(9, "</s>");
            closeTaker.put(8, "</code>");
            closeTaker.put(10, "</ins>");
            closeTaker.put(11, "</del>");
            closeTaker.put(7, "</q>");
            closeTaker.put(12, "</pre>");
            closeTaker.put(13, "</var>");
        }
    }

    {
        {
            taker.put("*", 1);
            taker.put("_", 2);
            taker.put("__", 3);
            taker.put("**", 4);
            taker.put("<<", 5);
            taker.put("}}", 6);
            taker.put("''", 7);
            taker.put("`", 8);
            taker.put("--", 9);
            taker.put(">>", 10);
            taker.put("{{", 11);
            taker.put("```", 12);
            taker.put("%", 13);
        }
    }

    private static final HashMap<Integer, String> puter = new HashMap<>();

    {
        {
            puter.put(1, "*");
            puter.put(2, "_");
            puter.put(3, "__");
            puter.put(4, "**");
            puter.put(5, "<<");
            puter.put(6, "}}");
            puter.put(7, "''");
            puter.put(8, "`");
            puter.put(9, "--");
            puter.put(10, ">>");
            puter.put(11, "{{");
            puter.put(12, "```");
            puter.put(13, "%");
        }
    }

    private static final HashMap<String, String> closer = new HashMap<>();

    {
        {
            closer.put("*", "*");
            closer.put("**", "**");
            closer.put("_", "_");
            closer.put("__", "__");
            closer.put(">>", "<<");
            closer.put("{{", "}}");
            closer.put("''", "''");
            closer.put("--", "--");
            closer.put("`", "`");
            closer.put("```", "```");
            closer.put("%", "%");
        }
    }

    private int i;
    private String s;
    private final int[] pointer = new int[1000000];

    private void maker(int length) {
        StrInt q = new StrInt(i, s.substring(i, i + length));
        String e;
        e = q.value;
        if (closer.containsKey(q.value)) {
            e = closer.get(q.value);
        }
        if (conditionChecker.get(e) == -1) {
            conditionChecker.put(e, 0);
            bas.add(q);
            i += length;
        } else {
            if (!q.value.equals("```")) {
                while (!bas.getLast().value.equals(closer.get(q.value))) {
                    conditionChecker.put(bas.getLast().value, -1);
                    bas.removeLast();
                }
                pointer[i] = taker.get(q.value);
                pointer[bas.getLast().place] = taker.get(bas.getLast().value);
                i += length;
                conditionChecker.put(bas.getLast().value, -1);
                bas.removeLast();
            } else {
                while (!bas.getLast().value.equals(closer.get(q.value))) {
                    conditionChecker.put(bas.getLast().value, -1);
                    bas.removeLast();
                }
                for (int j = bas.getLast().place + 1; j < i; j++) {
                    pointer[j] = -1;
                }
                pointer[i] = taker.get(q.value);
                pointer[bas.getLast().place] = taker.get(bas.getLast().value);
                conditionChecker.put(bas.getLast().value, -1);
                bas.removeLast();
                i += length;
            }
        }
    }

    private String run(String currentStr) {
        s = currentStr;
        bas.clear();
        conditionChecker.put("**", -1);
        conditionChecker.put("__", -1);
        conditionChecker.put("*", -1);
        conditionChecker.put("_", -1);
        conditionChecker.put("--", -1);
        conditionChecker.put("`", -1);
        conditionChecker.put(">>", -1);
        conditionChecker.put("}}", -1);
        conditionChecker.put("<<", -1);
        conditionChecker.put("{{", -1);
        conditionChecker.put("''", -1);
        conditionChecker.put("```", -1);
        conditionChecker.put("%", -1);
        int counter = 0;
        int check = 0;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) == '#') {
                counter++;
            } else if (s.charAt(i) != '#') {
                if (s.charAt(i) == ' ') {
                    check = 1;
                }
                break;
            }
        }
        String pref = "";
        String postf = "";
        if (check == 1 && counter != 0) {
            pref = "<h" + counter + ">";
            postf = "</h" + counter + ">";
            s = s.substring(counter + 1);
        } else {
            pref = "<p>";
            postf = "</p>";
        }
        StringBuilder base = new StringBuilder();
        base.append(pref);
        for (int j = 0; j < s.length(); j++) {
            pointer[j] = -1;
        }
        i = 0;
        while (i < s.length()) {
            if ((i == 0 || s.charAt(i - 1) != '\\') && ((taker.containsKey(s.substring(i, i + 1)))) ||
                    (i < s.length() - 1 && taker.containsKey(s.substring(i, i + 2))) ||
                    (i < s.length() - 2 && taker.containsKey(s.substring(i, i + 3)))) {
                if (taker.containsKey(s.charAt(i) + "") && (i == s.length() - 1 ||
                        i < s.length() - 1 && !taker.containsKey(s.substring(i, i + 2))) &&
                        (i == s.length() - 2 || i < s.length() - 2 && !taker.containsKey(s.substring(i, i + 3))) ||
                        i == s.length() - 1) {
                    maker(1);
                } else if (i <= s.length() - 2 && taker.containsKey(s.substring(i, i + 2) + "") &&
                        (i == s.length() - 2 || !taker.containsKey(s.substring(i, i + 3)))) {
                    maker(2);
                } else if (i <= s.length() - 3 && taker.containsKey(s.substring(i, i + 3))) {
                    maker(3);
                }
            } else {
                i++;
            }
        }
        base.append(toStr(s));
        base.append(postf);
        return base.toString();
    }


    public StringBuilder toStr(String curs) {
        condition.put(1, 0);
        condition.put(2, 0);
        condition.put(3, 0);
        condition.put(4, 0);
        condition.put(5, 0);
        condition.put(6, 0);
        condition.put(7, 0);
        condition.put(8, 0);
        condition.put(9, 0);
        condition.put(10, 0);
        condition.put(11, 0);
        condition.put(12, 0);
        condition.put(13, 0);
        int j = 0;
        StringBuilder base = new StringBuilder();
        while (j < s.length()) {
            if (pointer[j] == -1 && curs.charAt(j) != '\\') {
                if (curs.charAt(j) != '&' && curs.charAt(j) != '>' && curs.charAt(j) != '<' && curs.charAt(j) != '&') {
                    base.append(curs.charAt(j));
                } else if (curs.charAt(j) == '&') {
                    base.append("&amp;");
                } else if (curs.charAt(j) == '<') {
                    base.append("&lt;");
                } else {
                    base.append("&gt;");
                }
                j++;

            } else if (curs.charAt(j) == '\\' && !taker.containsKey(curs.substring(j + 1))) {
                j++;
            } else if (pointer[j] != -1) {
                if (puter.get(pointer[j]).equals(closer.get(puter.get(pointer[j])))) {
                    if (condition.get(pointer[j]) % 2 == 0) {
                        base.append(openTaker.get(pointer[j]));
                        condition.put(pointer[j], 1);
                    } else if (condition.get(pointer[j]) % 2 != 0) {
                        base.append(closeTaker.get(pointer[j]));
                        condition.put(pointer[j], 0);
                    }
                } else {
                    if (openTaker.containsKey(pointer[j])) {
                        base.append(openTaker.get(pointer[j]));
                    } else {
                        base.append(closeTaker.get(pointer[j]));
                    }
                }
                j += puter.get(pointer[j]).length();
            } else j++;
        }
        return base;
    }

    public void parse() throws IOException {
        String s = reader.readLine();
        while (true) {
            while (s == null || s.length() == 0) {
                s = reader.readLine();
                if (s == null) {
                    break;
                }
            }
            if (s == null) break;

            StringBuilder e = new StringBuilder();
            while (s.length() != 0) {
                e.append(s);
                s = reader.readLine();
                if (s == null) {
                    break;
                } else {
                    e.append(System.lineSeparator());
                }
            }
            int q = e.length() - 1;
            while (e.charAt(q) == '\n' || e.charAt(q) == '\r') {
                q--;
            }
            e = new StringBuilder(e.substring(0, q + 1));
            writer.write(run(e.toString()) + System.lineSeparator());

            s = reader.readLine();
        }
        close();
    }

    @Override
    public void close() throws IOException {
        reader.close();
        writer.close();
    }
}
