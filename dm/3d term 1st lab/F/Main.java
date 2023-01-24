import java.util.ArrayList;
import java.util.Scanner;

public class Main {
    static class Pair {
        int first;
        int second;

        Pair(int f, int s) {
            this.first = f;
            this.second = s;
        }

    }

    static ArrayList<Pair> decode(ArrayList<Integer> code) {
        int n = code.size() + 2;
        ArrayList<Integer> nodeToDegree = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            nodeToDegree.add(1);
        }
        for (int i = 0; i < n - 2; ++i)
            nodeToDegree.set(code.get(i), 1 + nodeToDegree.get(code.get(i)));

        int globalIterator = 0;
        while (globalIterator < n && nodeToDegree.get(globalIterator) != 1) {
            ++globalIterator;
        }
        int node = globalIterator;

        ArrayList<Pair> tree = new ArrayList<>();
        for (int i = 0; i < n - 2; ++i) {
            int v = code.get(i);
            tree.add(new Pair(node, v));

            nodeToDegree.set(node, nodeToDegree.get(node) - 1);
            nodeToDegree.set(v, -1 + nodeToDegree.get(v));
            if (nodeToDegree.get(v) == 1 && v < globalIterator)
                node = v;
            else {
                ++globalIterator;
                while (globalIterator < n && nodeToDegree.get(globalIterator) != 1)
                    ++globalIterator;
                node = globalIterator;
            }
        }
        for (int v = 0; v < n - 1; ++v)
            if (nodeToDegree.get(v) == 1)
                tree.add(new Pair(v, n - 1));
        return tree;
    }

    public static void main(String[] args) {
        Scanner scn = new Scanner(System.in);
        int n;
        n = scn.nextInt();
        ArrayList<Integer> argues = new ArrayList<>();
        for (int i = 0; i < n - 2; i++) {
            argues.add(scn.nextInt() - 1);
        }
        ArrayList<Pair> res = decode(argues);
        for (Pair j : res) {
            System.out.println((j.first + 1) + " " + (j.second + 1));
        }
    }
}