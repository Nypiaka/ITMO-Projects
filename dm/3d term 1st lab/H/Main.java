import java.io.*;
import java.util.*;

class Pair {
    int first;
    int second;

    Pair(int f, int s) {
        this.first = Math.max(f, s);
        this.second = Math.min(f, s);
    }

    public boolean equals(Object obj) {
        if (obj instanceof Pair) {
            Pair pp = (Pair) obj;
            return (pp.first == (this.first) && pp.second == this.second);
        } else {
            return false;
        }
    }

    public int hashCode() {
        return first % 6 + second * 3 / 2;
    }
}

class G {
    int N;
    int size;
    public HashSet<Pair> edges;


    public G(G g, int first, int second) {
        this.size = g.size;
        this.N = g.N;
        this.edges = new HashSet<>();
        for (Pair current : g.edges) {
            if (!(current.first == first && current.second == second)) {
                this.edges.add(new Pair(current.first, current.second));
            }
        }
    }

    public G(G g, int first, int second, boolean meow) {
        this.size = g.size;
        this.N = g.N;
        this.edges = new HashSet<>();
        for (Pair current : g.edges) {
            if (current.first != second && current.second != second)
                this.edges.add(new Pair(current.first, current.second));
            else {
                if (current.first == second || current.first != first) {
                    if (current.first == second) {
                        this.edges.add(new Pair(first, current.second));
                    } else this.edges.add(new Pair(first, current.first));
                }
            }
        }
    }

    public G(int n) {
        this.N = n;
        this.size = n;
        this.edges = new HashSet<>();
    }

}

public class Main {
    static boolean empty(G G) {
        return G.edges.size() == 0;
    }

    static G remove_edge(G G, int first, int second) {
        G resultGraph = new G(G, first, second);
        return resultGraph;
    }

    static G glue_nodes(G G, int first, int second) {
        G resultGraph = new G(G, first, second, true);
        resultGraph.size--;
        return resultGraph;
    }


    static Pair find(G G) {
        return G.edges.stream().findFirst().get();
    }

    static int[] chromo_empty(int tree_size) {
        int[] result = new int[tree_size + 1];
        result[0] = 1;
        return result;
    }

    static int[] P(G G) {
        if (empty(G)) {
            return chromo_empty(G.size);
        } else {
            Pair res_pair = find(G);
            int[] first = P(remove_edge(G, res_pair.first, res_pair.second));
            int[] second = P(glue_nodes(G, res_pair.first, res_pair.second));
            int first_iterator = first.length - 1;
            int second_iterator = second.length - 1;
            while (second_iterator >= 0) {
                first[first_iterator] -= second[second_iterator];
                first_iterator--;
                second_iterator--;
            }
            return first;
        }
    }


    static int[] read(String NM) {
        StringBuilder f = new StringBuilder();
        StringBuilder s = new StringBuilder();
        boolean first = true;
        for (int i = 0; i < NM.length(); i++) {
            if (Character.isDigit(NM.charAt(i)) && first) {
                f.append(NM.charAt(i));
            } else if (Character.isWhitespace(NM.charAt(i))) {
                first = false;
            } else if (!first && Character.isDigit(NM.charAt(i))) {
                s.append(NM.charAt(i));
            }
        }
        return new int[]{Integer.parseInt(f.toString()), Integer.parseInt(s.toString())};
    }

    public static void main(String[] args) throws IOException {
        int n;
        int m;
        BufferedReader scanner = new BufferedReader(new InputStreamReader(System.in));
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(System.out));
        String NM = scanner.readLine();
        int[] r = read(NM);
        n = r[0];
        m = r[1];
        G G = new G(n);
        for (int i = 0; i < m; i++) {
            int[] r1 = read(scanner.readLine());
            int first = r1[0];
            int second = r1[1];
            G.edges.add(new Pair(first - 1, second - 1));
        }
        int[] res = P(G);
        System.out.println(res.length - 1);
        for (long i : res) {
            out.write((int) i + " ");
        }
        out.close();
    }
}