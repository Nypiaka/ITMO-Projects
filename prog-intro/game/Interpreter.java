package game;

import java.util.Scanner;

public class Interpreter {
    public int interpreter() {
        Scanner sc = new Scanner(System.in);
        String s = sc.nextLine();
        int pointer = 0;
        int pointer1 = s.length() - 1;
        for (int i = 0; i < s.length(); i++) {
            if (s.charAt(i) != ' ') {
                pointer = i;
                break;
            }
        }
        for (int i = s.length() - 1; i >= 0; i--) {
            if (s.charAt(i) != ' ') {
                pointer1 = i;
                break;
            }
        }
        s = s.substring(pointer, pointer1 + 1);
        if (s.matches("\\d+")) return Integer.parseInt(s);
        else return -1;
    }
}
