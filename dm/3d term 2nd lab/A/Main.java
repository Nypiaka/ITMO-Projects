import java.io.*;
import java.math.BigInteger;
import java.util.*;

class Pair implements Comparable {
    Object first;
    Object second;

    Pair(int f, int s) {
        this.first = f;
        this.second = s;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof Pair) {
            Pair pp = (Pair) obj;
            return (pp.first == (this.first) && pp.second == this.second);
        } else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.first, this.second);
    }

    @Override
    public String toString() {
        return "<" + this.first.toString() + ", " + this.second.toString() + ">";
    }

    @Override
    public int compareTo(Object o) {
        return Integer.compare((int) ((Pair) o).first, (int) this.first);
    }
}

public class Main {

    public static void main(String[] args) throws IOException {
        BufferedReader reader = new BufferedReader(new FileReader("schedule.in"));
        BufferedWriter writer = new BufferedWriter(new FileWriter("schedule.out"));
        int num = Integer.parseInt(reader.readLine());
        Pair[] data = new Pair[num];
        for (int i = 0; i < num; i++) {
            String[] r = reader.readLine().split(" ");
            data[i] = (new Pair(Integer.parseInt(r[1]), Integer.parseInt(r[0])));
        }
        reader.close();
        Arrays.sort(data);
        ArrayList<Integer> dates = new ArrayList<>();
        for (int i = 1; i < num + 1; i++) {
            dates.add(i);
        }
        long penalty = 0;
        for (Pair iter : data) {
            int res = Collections.binarySearch(dates, (int) iter.second);
            if (res == -1) {
                penalty += (long) ((int) iter.first);
            } else {
                if (res >= 0) {
                    dates.remove(res);
                } else {
                    dates.remove(-res - 2);
                }
            }
        }
        writer.write(String.valueOf(penalty));
        writer.close();
    }
}