import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;

public class Main {
    static long MOD = 1000000000 + 7;

    static long[][] pow(long[][] a, long[][] b) {
        long[][] result = new long[a.length][b.length];
        for (int row = 0; row < a.length; row++) {
            for (int col = 0; col < b.length; col++) {
                long res = 0;
                for (int inner = 0; inner < a.length; inner++) {
                    res = (res + (a[row][inner] % MOD * b[inner][col] % MOD) % MOD) % MOD;
                }
                result[row][col] = res;
            }
        }
        return result;
    }

    public static void main(String[] args) throws IOException {
        List<HashMap<Character, HashSet<Integer>>> base = new ArrayList<>();
        List<HashMap<Character, Integer>> itogoviy_dka = new ArrayList<>();
        int n, m, k, l;
        HashSet<Integer> dopusk = new HashSet<>();
        HashSet<HashSet<Integer>> znacheniya_kotorye_uzhe_byli_v_ocheredi = new HashSet<>();
        HashMap<HashSet<Integer>, Integer> sootvetstvie = new HashMap<>();
        Scanner scn = new Scanner(new File("problem5.in"));
        FileWriter writer = new FileWriter("problem5.out", false);
        n = scn.nextInt();
        for (int i = 0; i < n; i++) {
            base.add(new HashMap<>());
        }
        m = scn.nextInt();
        k = scn.nextInt();
        l = scn.nextInt();
        for (int i = 0; i < k; i++) {
            dopusk.add(scn.nextInt() - 1);
        }
        for (int i = 0; i < m; i++) {
            int start;
            int end;
            char mean;
            start = scn.nextInt();
            end = scn.nextInt();
            mean = scn.next().charAt(0);
            if (!base.get(start - 1).containsKey(mean)) {
                base.get(start - 1).put(mean, new HashSet<>());
            }
            base.get(start - 1).get(mean).add(end - 1);
        }
        Queue<HashSet<Integer>> queue = new ArrayDeque<>();
        HashSet<Integer> first = new HashSet<>();
        first.add(0);
        queue.add(first);
        int marker = 0;
        sootvetstvie.put(first, 0);
        HashSet<Integer> itogoviy_dopusk = new HashSet<>();
        while (!queue.isEmpty()) {
            HashSet<Integer> current = queue.remove();
            int current_marker = sootvetstvie.get(current);
            while (itogoviy_dka.size() <= current_marker) {
                itogoviy_dka.add(new HashMap<>());
            }
            HashMap<Character, HashSet<Integer>> current_alphabet = new HashMap<>();
            for (Integer iter : current) {
                HashMap<Character, HashSet<Integer>> tekuchka = base.get(iter);
                for (Character key : tekuchka.keySet()) {
                    for (Integer vals : tekuchka.get(key)) {
                        if (current_alphabet.containsKey(key)) {
                            current_alphabet.get(key).add(vals);
                        } else {
                            current_alphabet.put(key, new HashSet<>());
                            current_alphabet.get(key).add(vals);
                        }
                    }
                }
            }
            for (HashSet<Integer> values : current_alphabet.values()) {
                if (!znacheniya_kotorye_uzhe_byli_v_ocheredi.contains(values)) {
                    queue.add(values);
                    znacheniya_kotorye_uzhe_byli_v_ocheredi.add(values);
                    marker++;
                    sootvetstvie.put(values, marker);
                    for (Integer iterator : values) {
                        if (dopusk.contains(iterator)) {
                            itogoviy_dopusk.add(marker);
                            break;
                        }
                    }
                }
            }
            for (Character iter : current_alphabet.keySet()) {
                itogoviy_dka.get(current_marker).put(iter, sootvetstvie.get(current_alphabet.get(iter)));
            }
        }
        long[][] dka_table = new long[itogoviy_dka.size()][itogoviy_dka.size()];
        long[][] result = new long[itogoviy_dka.size()][itogoviy_dka.size()];
        for (int i = 0; i < itogoviy_dka.size(); i++) {
            for (Integer iter : itogoviy_dka.get(i).values()) {
                dka_table[i][iter] = (dka_table[i][iter] + 1) % MOD;
            }
            result[i][i] = 1;
        }

        int deg = l;
        while (deg > 0) {
            if (deg % 2 == 0) {
                deg /= 2;
                dka_table = pow(dka_table, dka_table);
            } else {
                deg--;
                result = pow(result, dka_table);
            }
        }
        long kolvo = 0;
        for (Integer i : itogoviy_dopusk) {
            kolvo = (kolvo + result[0][i]) % MOD;
        }
        writer.write(Long.toString(kolvo));
        writer.close();
    }
}