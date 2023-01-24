import java.util.ArrayList;
import java.util.Scanner;

public class Main {
    static int n;
    static ArrayList<ArrayList<Integer>> g = new ArrayList<>();
    static ArrayList<Integer> nodeToParent = new ArrayList<>();
    static ArrayList<Integer> curDegree = new ArrayList<>();

    static void dfs(int v) {
        for (int i = 0; i < g.get(v).size(); ++i) {
            int to = g.get(v).get(i);
            if (to != nodeToParent.get(v)) {
                nodeToParent.set(to, v);
                dfs(to);
            }
        }
    }

    static ArrayList<Integer> prufer_code() {
        nodeToParent.set(n - 1, -1);
        dfs(n - 1);

        int globalIterator = -1;
        for (int i = 0; i < n; ++i) {
            curDegree.set(i, g.get(i).size());
            if (curDegree.get(i) == 1 && globalIterator == -1)
                globalIterator = i;
        }

        ArrayList<Integer> resultArray = new ArrayList<>();
        int node = globalIterator;
        for (int currentIterator = 0; currentIterator < n - 2; ++currentIterator) {
            int parent = nodeToParent.get(node);
            resultArray.add(parent);
            curDegree.set(parent, curDegree.get(parent) - 1);
            if (curDegree.get(parent) == 1 && parent < globalIterator)
                node = parent;
            else {
                ++globalIterator;
                while (globalIterator < n && curDegree.get(globalIterator) != 1)
                    ++globalIterator;
                node = globalIterator;
            }
        }
        return resultArray;
    }


    public static void main(String[] args) {
        Scanner scn = new Scanner(System.in);
        n = scn.nextInt();
        for (int i = 0; i < n; i++) {
            g.add(new ArrayList<>());
            nodeToParent.add(0);
            curDegree.add(0);
        }
        for (int i = 0; i < n - 1; i++) {
            int node1;
            int node2;
            node1 = scn.nextInt();
            node2 = scn.nextInt();
            g.get(node1 - 1).add(node2 - 1);
            g.get(node2 - 1).add(node1 - 1);
        }
        ArrayList<Integer> res = prufer_code();
        for (int j : res) {
            System.out.print((j + 1) + " ");
        }
    }
}