package search;

public class BinarySearchShift {
    //PRED: size(a) > 0 && a[l] > a[r] && Exists R:((R == 0 && a[R] <= a[size(a)-1]) || (R == 1 && a[R] < a[0]) || (a[R-1] < a[R] < a[R+1]))
    //POST: ((R == 0 && a[R] <= a[size(a)-1]) || (a[R] < a[R-1]))
    //INV: a[R] > a[R - 1]
    public static int recursionBinSearch(int[] a, int l, int r) {
        int m = (l + r) / 2;
        if (m == a.length - 1) {
            //m == (l + r) / 2 && m == size(a) - 1 && (a[m] > a[m - 1]) => (a[R] < a[R-1])
            return m;
        }
        //m != size(a) - 1
        if (a[0] < a[a.length - 1]) {
            //a sorted true => R == 0
            return 0;
        }
        if (m > 0 && a[m] < a[m - 1]) {
            //R found true
            return m;
        }
        //R not found
        if ((a[0] > a[m])) {
            //a[0] > a[m] && R not found => r := m
            return recursionBinSearch(a, l, m);
        } else
            //a[0] < a[m] && R not found => l := m
            return recursionBinSearch(a, m, r);
    }
    
    //PRED: size(a) > 0 && Exists R:((R == 0 && a[R] <= a[size(a)-1]) || (R == 1 && a[R] < a[0]) || (a[R-1] < a[R] < a[R+1]))
    //POST: ((R == 0 && a[R] <= a[size(a)-1]) || (a[R] < a[R-1]))
    public static int iterativeBinSearch(int[] a) {
        if (a[0] < a[a.length - 1]) {
            //a sorted true => R == 0
            return 0;
        }
        int l = -1;
        int r = a.length;
        int m = (l + r) / 2;
        //INV: a[m] < a[m-1]
        while ((m != a.length - 1) && !(m > 0 && a[m] < a[m - 1])) {
            m = (l + r) / 2;
            if ((a[0] > a[m])) {
                //a[0] > a[m] && R not found => r := m
                r = m;
            } else
                //a[0] < a[m] && R not found => l := m
                l = m;
        }
        //(m == a.length - 1) || (m > 0 && a[m] < a[m - 1])
        //(m == a.length - 1) || (m > 0 && a[m] < a[m - 1]) => ((R == 0 && a[R] < a[size(a)-1]) || (a[R] <= a[R-1]))
        return m;
    }

    public static void main(String[] args) {
        int[] a = new int[args.length];
        for (int i = 0; i < args.length; i++) {
            a[i] = Integer.parseInt(args[i]);
        }
        System.out.println(iterativeBinSearch(a));
    }
}
