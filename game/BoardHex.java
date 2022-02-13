package game;

import java.util.Scanner;

public class BoardHex extends AbstractBoard implements CharTaker {
    private final Interpreter boardHexInterpreter = new Interpreter();

    @Override
    protected char[][] board() {
        return this.board;
    }

    @Override
    protected void full(int M, int N) {
        Scanner sc = new Scanner(System.in);
        String s1;
        System.out.println("Введите размер гекса.");
        M = boardHexInterpreter.interpreter();
        while (M < 1) {
            M = boardHexInterpreter.interpreter();
        }
        N = M;
        this.board = new char[M][N];
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < M; j++) {
                board[i][j] = CharEmpty;
            }
        }
        this.drawSize = M * N;
    }

    @Override
    protected boolean checkWin(int x, int y, int K) {
        return (easyCheck(x, y, 1, 1, K) ||
                easyCheck(x, y, 1, 0, K) ||
                easyCheck(x, y, 0, 1, K));
    }

    @Override
    protected void printBoard() {
        int M = this.board.length;
        int column;
        int line;
        int counter1 = 0;
        int counter = M - 1;
        int[] arrayForPrint = new int[2 * M - 1];
        for (int i = 0; i < 2 * M - 1; i++) {
            arrayForPrint[i] = 0;
        }
        int trigger = 0;
        for (int i = 1; i < 2 * M; i++) {

            if (i == 1) {
                arrayForPrint[(2 * M - 1) / 2] = 1;
            } else {
                int l = 0;
                while (arrayForPrint[l] != 1) {
                    l++;
                }
                if (l != 0) {
                    l--;
                }
                int r = arrayForPrint.length - 1;
                while (arrayForPrint[r] != 1) {
                    r--;
                }
                if (r != arrayForPrint.length - 1) {
                    r++;
                }
                if (i - 1 <= M) {
                    for (int k = l; k <= r; k++) {
                        arrayForPrint[k] = 1 - arrayForPrint[k];
                    }
                } else {
                    for (int k = l + 1; k <= r - 1; k++) {
                        arrayForPrint[k] = 1 - arrayForPrint[k];
                    }
                }
            }
            if (M - i - 1 >= 0) {
                column = M - i;
                line = 0;
            } else {
                column = 0;
                line = i - M;
            }
            for (int h = Integer.toString(counter).length(); h < Integer.toString(M).length(); h++) {
                System.out.print(" ");
            }

            if (counter > 0 && trigger == 0) {
                counter1 = counter;
                counter--;
            } else if (counter == 0) {
                trigger = 1;
                counter1 = counter;
                counter++;
            } else if (counter > 0) {
                counter1 = counter;
                counter++;
            }
            for (int j = 0; j < 2 * M - 1; j++) {
                if (j == counter1) {
                    System.out.print(counter1);
                } else if (arrayForPrint[j] == 0) {
                    System.out.print(" ");
                }
                if (arrayForPrint[j] != 0) {
                    System.out.print(board[line][column]);
                    column++;
                    line++;
                }
            }
            System.out.println();
        }
    }
}
