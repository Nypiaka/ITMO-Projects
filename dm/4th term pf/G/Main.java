import java.math.BigInteger;
import java.util.*;
import java.util.stream.Collectors;

public class Main {

    static List<BigInteger> mult_poly(final List<BigInteger> first_poly,
                                      final List<BigInteger> second_poly) {
        ArrayList<BigInteger> mult = new ArrayList<>();
        for (int i = 0; i < first_poly.size() + second_poly.size() - 1; i++) {
            mult.add(BigInteger.ZERO);
        }
        for (int i = 0; i < first_poly.size(); i++) {
            for (int j = 0; j < second_poly.size(); j++) {
                mult.set(i + j, mult.get(i + j).add(first_poly.get(i).multiply(second_poly.get(j))));
            }
        }
        return mult.subList(0, 7);
    }

    static List<BigInteger> pow_poly(final List<BigInteger> poly, BigInteger pow) {
        if (pow.equals(BigInteger.ZERO)) {
            ArrayList<BigInteger> result = new ArrayList<>();
            result.add(BigInteger.ONE);
            for (int i = 1; i < 7; i++) {
                result.add(BigInteger.ZERO);
            }
            return result;
        }
        if (pow.mod(BigInteger.TWO).equals(BigInteger.ONE)) {
            return mult_poly(pow_poly(poly, pow.subtract(BigInteger.ONE)), poly).subList(0, 7);
        }
        List<BigInteger> tmp = pow_poly(poly, pow.divide(BigInteger.TWO));
        return mult_poly(tmp, tmp).subList(0, 7);
    }

    static List<BigInteger> add_poly(final List<BigInteger> first_poly,
                                     final List<BigInteger> second_poly) {
        ArrayList<BigInteger> result = new ArrayList<>();
        for (int i = 0; i < Math.max(first_poly.size(), second_poly.size()); i++) {
            BigInteger first = ((i < first_poly.size()) ? first_poly.get(i) : BigInteger.ZERO);
            BigInteger second = ((i < second_poly.size()) ? second_poly.get(i) : BigInteger.ZERO);
            result.add(first.add(second));
        }
        return result;
    }

    interface Combinatoric {
        List<BigInteger> getWeights();

        void calcWeightsForAll();
    }

    abstract static class AbstractCombinatoric implements Combinatoric {
        ArrayList<Combinatoric> args = new ArrayList<>();
        ArrayList<BigInteger> weights;

        public ArrayList<BigInteger> getWeights() {
            return weights;
        }

        public void calcWeightsForAll() {
            for (var combic : args) {
                combic.calcWeightsForAll();
            }
        }

    }

    static class B extends AbstractCombinatoric {
        public B() {
            this.weights = new ArrayList<>();
            weights.add(BigInteger.ZERO);
            weights.add(BigInteger.ONE);
            for (int i = 2; i < 7; i++) {
                weights.add(BigInteger.ZERO);
            }
        }

        @Override
        public void calcWeightsForAll() {
        }
    }

    static class L extends AbstractCombinatoric {
        public L(Combinatoric arg) {
            args.add(arg);
        }

        @Override
        public ArrayList<BigInteger> getWeights() {
            calcWeightsForAll();
            List<BigInteger> elem = new ArrayList<>(args.get(0).getWeights());
            elem.set(0, BigInteger.ZERO);
            var res = new ArrayList<>(
                    add_poly(pow_poly(elem, BigInteger.ZERO),
                            add_poly(elem,
                                    add_poly(pow_poly(elem, BigInteger.TWO),
                                            add_poly(pow_poly(elem, BigInteger.valueOf(3)),
                                                    add_poly(pow_poly(elem, BigInteger.valueOf(4)),
                                                            add_poly(pow_poly(elem, BigInteger.valueOf(5)), pow_poly(elem, BigInteger.valueOf(6)))))))));
            return res;
        }
    }

    static class P extends AbstractCombinatoric {
        public P(Combinatoric arg1, Combinatoric arg2) {
            args.add(arg1);
            args.add(arg2);
        }

        @Override
        public ArrayList<BigInteger> getWeights() {
            calcWeightsForAll();
            List<BigInteger> elem1 = args.get(0).getWeights();
            List<BigInteger> elem2 = args.get(1).getWeights();
            return new ArrayList<>(mult_poly(elem1, elem2));
        }
    }

    static class S extends AbstractCombinatoric {
        public S(Combinatoric arg) {
            args.add(arg);
        }

        @Override
        public ArrayList<BigInteger> getWeights() {
            calcWeightsForAll();
            ArrayList<BigInteger> elem = new ArrayList<>(args.get(0).getWeights());
            elem.set(0, BigInteger.ZERO);
            var s1 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE, BigInteger.ONE}).collect(Collectors.toList());
            var s2 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO, BigInteger.ONE}).collect(Collectors.toList());
            var s3 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE}).collect(Collectors.toList());
            var s4 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO}).collect(Collectors.toList());
            var s5 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE, BigInteger.ZERO}).collect(Collectors.toList());
            var s6 = Arrays.stream(new BigInteger[]{BigInteger.ONE, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ZERO, BigInteger.ONE}).collect(Collectors.toList());
            s1 = pow_poly(s1, elem.get(1));
            s2 = pow_poly(s2, elem.get(2));
            s3 = pow_poly(s3, elem.get(3));
            s4 = pow_poly(s4, elem.get(4));
            s5 = pow_poly(s5, elem.get(5));
            s6 = pow_poly(s6, elem.get(6));
            var pr = mult_poly(mult_poly(mult_poly(mult_poly(mult_poly(s1, s2), s3), s4), s5), s6);
            return new ArrayList<>(pr);
        }
    }

    static class Parser {
        static Combinatoric parse(String arg) {
            if (arg.charAt(0) == 'S') {
                return new S(Parser.parse(arg.substring(2, arg.length() - 1)));
            } else if (arg.charAt(0) == 'L') {
                return new L(Parser.parse(arg.substring(2, arg.length() - 1)));
            } else if (arg.charAt(0) == 'P') {
                int cur_index = 0;
                cur_index += 2;
                Combinatoric c1;
                Combinatoric c2;
                if (arg.charAt(cur_index) == 'B') {
                    c1 = new B();
                } else {
                    cur_index += 2;
                    int balance = 1;
                    while (balance != 0) {
                        if (arg.charAt(cur_index) == '(') balance++;
                        if (arg.charAt(cur_index) == ')') balance--;
                        cur_index++;
                    }
                    cur_index--;
                    c1 = parse(arg.substring(2, cur_index + 1));
                }
                int first_end = cur_index;
                int second_start = first_end + 1;
                if (arg.charAt(second_start) == 'B') {
                    c2 = new B();
                } else {
                    second_start += 2;
                    int balance = 1;
                    while (balance != 0) {
                        if (arg.charAt(second_start) == '(') balance++;
                        if (arg.charAt(second_start) == ')') balance--;
                        second_start++;
                    }
                    second_start--;
                    c2 = parse(arg.substring(first_end + 1, second_start + 1));
                }
                return new P(c1, c2);
            } else if (arg.charAt(0) == 'B') {
                return new B();
            }
            return null;
        }
    }

    public static void main(String[] args) {
        Scanner scn = new Scanner(System.in);
        String arg = scn.nextLine();
        arg = arg.replaceAll(",", "");
        var el = (Objects.requireNonNull(Parser.parse(arg))).getWeights();
        for (BigInteger e : el) {
            System.out.print(e + " ");
        }
    }
}