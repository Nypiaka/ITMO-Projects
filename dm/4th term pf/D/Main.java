import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class Fraction {
    BigInteger numerator;
    BigInteger denominator;

    Fraction(BigInteger num, BigInteger denom) {
        BigInteger gcd = num.abs().gcd(denom.abs());
        this.numerator = num.divide(gcd);
        this.denominator = denom.divide(gcd);
        if (this.numerator.compareTo(BigInteger.ZERO) >= 0 && this.denominator.compareTo(BigInteger.ZERO) < 0 ||
                this.denominator.compareTo(BigInteger.ZERO) >= 0 && this.numerator.compareTo(BigInteger.ZERO) < 0) {
            this.numerator = this.numerator.abs().negate();
            this.denominator = this.denominator.abs();
        }
        if (this.denominator.compareTo(BigInteger.ZERO) < 0 && this.numerator.compareTo(BigInteger.ZERO) < 0) {
            this.numerator = this.numerator.abs();
            this.denominator = this.denominator.abs();
        }
    }

    static Fraction multiply(Fraction a, Fraction b) {
        BigInteger anum = a.numerator.abs();
        BigInteger bnum = b.numerator.abs();
        BigInteger first_num_second_denom_gcd = (anum.gcd(b.denominator));
        BigInteger first_denom_second_num_gcd = (a.denominator.gcd(bnum));
        return new Fraction((a.numerator.divide(first_num_second_denom_gcd)).multiply(
                (b.numerator.divide((first_denom_second_num_gcd)))),
                (a.denominator.divide(first_denom_second_num_gcd)).multiply(
                        (b.denominator.divide(first_num_second_denom_gcd))));
    }

    static Fraction divide(Fraction a, Fraction b) {
        return multiply(a, new Fraction(b.denominator, b.numerator));
    }

    static Fraction add(Fraction a, Fraction b) {
        if (!a.numerator.equals(BigInteger.ZERO) && !b.numerator.equals(BigInteger.ZERO)) {
            BigInteger lcm = a.denominator.multiply(b.denominator).divide(a.denominator.gcd(b.denominator));
            BigInteger amn = lcm.divide(a.denominator);
            BigInteger bmn = lcm.divide(b.denominator);
            return new Fraction(amn.multiply(a.numerator).add(bmn.multiply(b.numerator)), lcm);
        } else {
            if (!a.numerator.equals(BigInteger.ZERO)) {
                return a;
            } else {
                return b;
            }
        }
    }

    static Fraction subtract(Fraction a, Fraction b) {
        return add(a, new Fraction(b.numerator.negate(), b.denominator));
    }

    @Override
    public String toString() {
        return numerator.toString() + "/" + denominator.toString();
    }
}


public class Main {
    static ArrayList<Fraction> system_solver(int n, ArrayList<ArrayList<Fraction>> mat) {
        ArrayList<Fraction> res = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            res.add(new Fraction(BigInteger.ZERO, BigInteger.ONE));
        }
        for (int i = 0; i < mat.size(); i++) {
            for (int j = i + 1; j < mat.size(); j++) {
                Fraction f = Fraction.divide(mat.get(j).get(i), mat.get(i).get(i));
                for (int k = 0; k < mat.size() + 1; k++) {
                    mat.get(j).set(k, Fraction.subtract(mat.get(j).get(k), Fraction.multiply(f, mat.get(i).get(k))));
                }
            }
        }
        for (int i = mat.size() - 1; i >= 0; i--) {
            Fraction sum = new Fraction(BigInteger.ZERO, BigInteger.ONE);
            for (int j = mat.size() - 1; j > i; j--) {
                sum = Fraction.add(sum, Fraction.multiply(res.get(j), mat.get(i).get(j)));
            }
            Fraction resres = Fraction.subtract(mat.get(i).get(mat.size()), sum);
            res.set(i, Fraction.divide(resres, mat.get(i).get(i)));
        }
        return res;
    }

    static long pow(long num, int b) {
        if (b == 0)
            return 1;
        if (b % 2 == 1)
            return pow(num, b - 1) * num;
        long tmp = pow(num, b / 2);
        return tmp * tmp;
    }

    public static void main(String[] args) throws IOException {
        int r, k;
        Scanner scn = new Scanner(System.in);
        r = scn.nextInt();
        k = scn.nextInt();
        List<Integer> p = new ArrayList<>();

        for (int i = 0; i < k + 1; i++) {
            p.add(scn.nextInt());
        }

        ArrayList<BigInteger> q = new ArrayList<>();
        for (int i = 0; i < k + 2; i++) {
            q.add(BigInteger.ZERO);
        }
        q.set(0, BigInteger.ONE);
        for (int i = 1; i < k + 2; i++) {
            q.set(i, q.get(i - 1).multiply(BigInteger.valueOf(-r)).multiply(BigInteger.valueOf(k - i + 2)).divide(BigInteger.valueOf(i)));
        }
        ArrayList<BigInteger> a = new ArrayList<>();
        for (int i = 0; i < k + 1; i++) {
            a.add(BigInteger.ZERO);
        }
        for (int i = 0; i < k + 1; i++) {
            BigInteger rem_part = BigInteger.ZERO;
            for (int j = 0; j < i; j++) {
                rem_part = rem_part.add(a.get(j).multiply(q.get(i - j)));
            }
            a.set(i, BigInteger.valueOf(p.get(i)).subtract(rem_part));
        }

        ArrayList<ArrayList<Fraction>> system = new ArrayList<>();
        for (int i = 0; i < k + 1; i++) {
            system.add(new ArrayList<>());
            for (int j = 0; j < k + 2; j++) {
                system.get(i).add(new Fraction(BigInteger.ZERO, BigInteger.ONE));
            }
        }
        for (int i = 0; i < k + 1; i++) {
            for (int j = 0; j <= k; j++) {
                system.get(i).set(j, new Fraction(BigInteger.valueOf(pow(i, j)), BigInteger.ONE));
            }
            long power = pow(r, i);
            system.get(i).set(k + 1, new Fraction(a.get(i), BigInteger.valueOf(power)));
        }
//        system.forEach(System.out::println);
        ArrayList<Fraction> resolve = system_solver(k + 1, system);
        for (Fraction fr : resolve) {
            System.out.print(fr.numerator + "/" + fr.denominator + " ");
        }

    }
}