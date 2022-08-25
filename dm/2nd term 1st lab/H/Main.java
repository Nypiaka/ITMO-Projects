import javax.swing.*;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class Main {
    static ArrayList<HashMap<Character, Integer>> base = new ArrayList<>();
    static ArrayList<HashSet<Integer>> antibase = new ArrayList<>();
    static HashSet<Integer> reached_from_start = new HashSet<>();
    static HashSet<Integer> reaches_terminal = new HashSet<>();
    static HashSet<Integer> dopusk = new HashSet<>();

    public static void dfs(int u) {
        reached_from_start.add(u);
        for (int iter : base.get(u).values()) {
            if (!reached_from_start.contains(iter)) {
                dfs(iter);
            }
        }
    }

    public static void anti_dfs(int u) {
        reaches_terminal.add(u);
        for (int iter : antibase.get(u)) {
            if (!reaches_terminal.contains(iter)) {
                anti_dfs(iter);
            }
        }
    }

    public static class Pair1 {
        public final HashSet<Integer> hs;
        public final Character symb;

        public Pair1(Character t, HashSet<Integer> u) {
            this.hs = u;
            this.symb = t;
        }
    }

    public static void main(String[] args) throws IOException {
        HashMap<String, HashSet<Integer>> Inv = new HashMap<>();
        HashSet<Character> alphabet = new HashSet<>();
        ArrayList<HashSet<Integer>> P = new ArrayList<>();
        int n, m, k;
        Scanner scn = new Scanner(new File("minimization.in"));
        FileWriter writer = new FileWriter("minimization.out", false);
        n = scn.nextInt();
        m = scn.nextInt();
        k = scn.nextInt();

        for (int i = 0; i < n; i++) {
            base.add(new HashMap<>());
            antibase.add(new HashSet<>());
        }
        for (int i = 0; i < k; i++) {
            dopusk.add(scn.nextInt() - 1);
        }
        for (int i = 0; i < m; i++) {
            int start = scn.nextInt() - 1;
            int end = scn.nextInt() - 1;
            char val = scn.next().charAt(0);
            antibase.get(end).add(start);
            base.get(start).put(val, end);
        }
        dfs(0);

        for (int iter : dopusk) {
            if (reached_from_start.contains(iter)) {
                anti_dfs(iter);
            }
        }
        ArrayList<HashMap<Character, Integer>> true_base = new ArrayList<>();
        HashMap<Integer, Integer> sootv = new HashMap<>();
        HashMap<Integer, Integer> antisootv = new HashMap<>();
        int mrk = 0;
        for (int i = 0; i < n; i++) {
            if (reaches_terminal.contains(i) && reached_from_start.contains(i)) {
                sootv.put(i, mrk);
                antisootv.put(mrk, i);
                mrk++;
                true_base.add(new HashMap<>());
            }
        }
        for (int i = 0; i < n; i++) {
            if (sootv.containsKey(i)) {
                for (char symb : base.get(i).keySet()) {
                    if (sootv.containsKey(base.get(i).get(symb))) {
                        true_base.get(sootv.get(i)).put(symb, sootv.get(base.get(i).get(symb)));
                        alphabet.add(symb);
                    }
                }
            }
        }
        for (int i = 0; i < true_base.size(); i++) {
            for (char iter : true_base.get(i).keySet()) {
                if (!Inv.containsKey(iter + " " + true_base.get(i).get(iter))) {
                    Inv.put(iter + " " + true_base.get(i).get(iter), new HashSet<>());
                    Inv.get(iter + " " + true_base.get(i).get(iter)).add(i);
                } else {
                    Inv.get(iter + " " + true_base.get(i).get(iter)).add(i);
                }
            }
        }
        HashSet<Integer> true_dopusk = new HashSet<>();
        for (int iter : dopusk) {
            if (reached_from_start.contains(iter)) {
                true_dopusk.add(sootv.get(iter));
            }
        }
        int[] Class = new int[sootv.size()];
        if (true_dopusk.size() > 0) {
            P.add(new HashSet<>(true_dopusk));
        }
        HashSet<Integer> antidopusk = new HashSet<>();
        for (int i = 0; i < sootv.size(); i++) {
            if (!true_dopusk.contains(i)) {
                antidopusk.add(i);
                Class[i] = 1;
            } else {
                Class[i] = 0;
            }
        }
        if (antidopusk.size() > 0) {
            P.add(new HashSet<>(antidopusk));
        }
        Queue<Pair1> queue = new ArrayDeque<>();
        for (Character c : alphabet) {
            queue.add(new Pair1(c, P.get(0)));
            if (P.size() > 1) {
                queue.add(new Pair1(c, P.get(1)));
            }
        }
        while (!queue.isEmpty()) {
            HashSet<Integer> C = queue.peek().hs;
            Character a = queue.peek().symb;
            queue.remove();
            HashMap<Integer, HashSet<Integer>> Involved = new HashMap<>();
            for (Integer q : C) {
                if (Inv.containsKey(a + " " + q)) {
                    for (Integer r : Inv.get(a + " " + q)) {
                        Integer i = Class[r];
                        if (!Involved.containsKey(i)) {
                            Involved.put(i, new HashSet<>());
                        }
                        Involved.get(i).add(r);
                    }
                }
            }
            for (Integer i1 : Involved.keySet()) {
                if (Involved.get(i1).size() < P.get(i1).size()) {
                    P.add(new HashSet<>());
                    int j = P.size() - 1;
                    for (Integer r1 : Involved.get(i1)) {
                        P.get(i1).remove(r1);
                        P.get(j).add(r1);
                    }
                    if (P.get(j).size() > P.get(i1).size()) {
                        HashSet<Integer> cur = P.get(i1);
                        P.set(i1, P.get(j));
                        P.set(j, cur);
                    }
                    for (Integer r2 : P.get(j)) {
                        Class[r2] = j;
                    }
                    for (Character c : alphabet) {
                        queue.add(new Pair1(c, P.get(j)));
                    }
                }
            }
        }
        for (int i = 0; i < P.size(); i++) {
            if (P.get(i).contains(0)) {
                HashSet<Integer> swaper = P.get(i);
                P.set(i, P.get(0));
                P.set(0, swaper);
                for (Integer zero_it : P.get(0)) {
                    Class[zero_it] = 0;
                }
                for (Integer swap : P.get(i)) {
                    Class[swap] = i;
                }
                break;
            }
        }
        ArrayList<HashMap<Character, Integer>> result_base = new ArrayList<>();
        for (int i = 0; i < P.size(); i++) {
            result_base.add(new HashMap<>());
        }
        int result_iter = 0;
        HashSet<Integer> result_dopusk = new HashSet<>();
        for (HashSet<Integer> iter : P) {
            result_base.set(result_iter, new HashMap<>());
            for (Character cur_char : true_base.get(P.get(result_iter).iterator().next()).keySet()) {
                result_base.get(result_iter).put(cur_char, Class[true_base.get(P.get(result_iter).iterator().next()).get(cur_char)]);
            }
            for (Integer it : iter) {
                if (true_dopusk.contains(it)) {
                    result_dopusk.add(result_iter);
                    break;
                }
            }
            result_iter++;
        }
        int rebra = 0;
        for (HashMap<Character, Integer> characterIntegerHashMap : result_base) {
            rebra += characterIntegerHashMap.size();
        }
        writer.write(result_base.size() + " " + rebra + " " + result_dopusk.size() + "\n");
        for (Integer dop : result_dopusk) {
            writer.write((dop + 1) + " ");
        }
        writer.write("\n");
        for (int i = 0; i < result_base.size(); i++) {
            for (Character a : result_base.get(i).keySet()) {
                writer.write((i + 1) + " " + (result_base.get(i).get(a) + 1) + " " + a + "\n");
            }
        }
        writer.close();
    }
}