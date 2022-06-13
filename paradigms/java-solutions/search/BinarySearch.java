package search;


public class BinarySearch {
    // :NOTE: pre-condition too strict, only a[l..r) should be sorted
    // Pred: size(a) > 0 && for all i: (0 <= i < size(a) - 2) { a[i] >= a[i - 1] } && a[-1] = Integer.MaxValue &&
    // a[size(a)] = Integer.MinValue && l <= r + 1 && -1 <= l < size(a) && 0 <= R <= size(a)
    // Post: a[R] <= obj && R == min ( all R`: a[R`] <= obj && (-1 < R` <= size(a)))
    // Inv: l < r && a[l] > obj >= a[r]
    public static int recursionBinSearch(int[] a, int l, int r, int obj) {
        if (l == r - 1) {
            // (a[l] > obj >= a[R] && l == R - 1) => R == min (all R`: a[R`] <= obj && (-1 < R` <= size(a)))
            return r;
        }
        // l < r - 1 && a[l] > obj >= a[r]
        int m = (l + r) / 2;
        // m = (l + r) / 2 && l < r - 1 && a[l] > obj >= a[r]
        if (a[m] > obj) {
            // a[m] > obj && m = (l + r) / 2 && l < r - 1 && a[l] > obj >= a[r]
            return recursionBinSearch(a, m, r, obj);
        } else
            // a[m] <= obj && m = (l + r) / 2 && l < r - 1 && a[l] > obj >= a[r]
            return recursionBinSearch(a, l, m, obj);
    }

    // Pred: size(a) > 0 && for all i: (0 <= i<size(a) - 2) { a[i] >= a[i-1] } && a[-1] = Integer.MaxValue && a[size(a)] = Integer.MinValue
    // Post: a[R] <= obj && R == min ( all R`: a[R`] <= obj && (-1 < R` <= size(a)))
    public static int iterativeBinSearch(int[] a, int obj) {
        int l = -1;
        int r = a.length;
        // l = -1 && r = size(a) && size(a) > 0 && for all i: (0 <= i < size(a) - 2)
        // { a[i] >= a[i-1] } && a[-1] = Integer.MaxValue && a[size(a)] = Integer.MinValue
        // Inv: a[l] > obj >= a[r]
        while (l < r - 1) {
            // a[l] > obj >= a[r] && l < r - 1
            int m = (l + r) / 2;
            // m = (l + r) / 2 && a[l] > obj >= a[r] && l < r - 1
            if (a[m] > obj) {
                // a[m] > obj && m = (l + r) / 2 && a[l] > obj >= a[r] && l < r - 1
                l = m;
                // (l = m && m = (l + r) / 2) => l < r => a[l] > obj >= a[r]
                // a[l] > obj >= a[r] && l < r
            } else
                // a[m] <= obj && m = (l + r) / 2 && a[l] > obj >= a[r] && l < r - 1
                r = m;
            // (r = m && m = (l + r) / 2) => l < r => a[l] > obj >= a[r]
            // a[l] > obj >= a[r] && l < r
            // a[l] > obj >= a[r] && l < r
            // :NOTE: nothing about loop termination
        }
        // a[l] > obj >= a[r] && (l < r && r <= l + 1) => (l = r + 1)
        // (a[l] > obj >=a[r]) && (l = r + 1) => R == min ( all R`: a[R`] <= obj && (-1 < R` <= size(a)))
        return r;
    }

    // :NOTE: no pre-/post- conditions
    public static void main(String[] args) {
        int[] a = new int[args.length - 1];
        for (int i = 1; i < args.length; i++) {
            a[i - 1] = Integer.parseInt(args[i]);
        }
        // :NOTE: only one method is tested
        System.out.println(recursionBinSearch(a, -1, a.length, Integer.parseInt(args[0])));
    }
}
