import java.io.*;
import java.util.*;

public class Main {

    public static HashSet<Integer> generateAllSubsets(int original) {
        HashSet<Integer> allSubsets = new HashSet<>();
        allSubsets.add(0);
        String curNum = Integer.toBinaryString(original);
        for (int i = 0; i < curNum.length(); i++) {
            if ((curNum.charAt(i)) == '1') {
                HashSet<Integer> tempClone = new HashSet<>(allSubsets);
                for (int subset : tempClone) {
                    int extended = subset;
                    double pow = Math.pow(2, curNum.length() - i - 1);
                    if (extended + pow <= original) {
                        extended += pow;
                    }
                    allSubsets.add(extended);
                }
            }
        }
        return allSubsets;
    }

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("check.in"));
        BufferedWriter writer = new BufferedWriter(new FileWriter("check.out"));
        String[] curTwoNums = reader.readLine().split(" ");
        int num = Integer.parseInt(curTwoNums[1]);
        HashSet<Integer> mainSet = new HashSet<>();
        for (int i = 0; i < num; i++) {
            String[] curNums = reader.readLine().split(" ");
            int curSet = 0;
            for (int j = 1; j < curNums.length; j++) {
                curSet += Math.pow(2, Integer.parseInt(curNums[j]));
            }
            mainSet.add(curSet);
        }
        if (!mainSet.contains(0)) {
            writer.write("NO");
            writer.close();
            return;
        }
        for (int curSet : mainSet) {
            HashSet<Integer> curSubSets = generateAllSubsets(curSet);
            for (int curSubSet : curSubSets) {
                if (!mainSet.contains(curSubSet)) {
                    writer.write("NO");
                    writer.close();
                    return;
                }
            }
        }
        for (int a : mainSet) {
            for (int b : mainSet) {
                if (Integer.bitCount(a) > Integer.bitCount(b)) {
                    boolean checker = false;
                    String curNum = Integer.toBinaryString(a);
                    for (int curInA = 0; curInA < curNum.length(); curInA++) {
                        double pow = Math.pow(2, curNum.length() - 1 - curInA);
                        if (curNum.charAt(curInA) == '1' && ((b | (int) pow) != b)) {
                            b += (int) pow;
                            checker |= mainSet.contains(b);
                            b -= (int) pow;
                        }
                    }
                    if (!checker) {
                        writer.write("NO");
                        writer.close();
                        return;
                    }
                }
            }
        }
        writer.write("YES");
        writer.close();
        return;
    }
}