package game;

import java.util.Scanner;

public class BoardClassic extends AbstractBoard implements CharTaker, Error {
    private final Interpreter boardClassicInterpreter = new Interpreter();

    @Override
    protected char[][] board() {
        return board;
    }

    @Override
    protected void full(int M, int N) {
        System.out.println("Введите высоту поля.");
        M = boardClassicInterpreter.interpreter();
        while (M < 1) {
            System.out.println(ERROR);
            M = boardClassicInterpreter.interpreter();
        }
        System.out.println("Введите длину поля.");
        N = boardClassicInterpreter.interpreter();
        while (N < 1) {
            System.out.println(ERROR);
            N = boardClassicInterpreter.interpreter();
        }
        this.board = new char[M][N];
        for (int i = 0; i < M; i++) {
            for (int j = 0; j < N; j++) {
                this.board[i][j] = CharEmpty;
            }
        }
        this.drawSize = M * N;
    }

    @Override
    protected boolean checkWin(int x, int y, int K) {
        return (easyCheck(x, y, 1, 1, K) ||
                easyCheck(x, y, 1, -1, K) ||
                easyCheck(x, y, 1, 0, K) ||
                easyCheck(x, y, 0, 1, K));
    }


    @Override
    protected void printBoard() {
        int M = this.board.length;
        int N = this.board[0].length;
        for (int i = 0; i < Integer.toString(M).length(); i++) {
            System.out.print(" ");
        }
        int maxLength = Integer.toString(N).length();
        for (int g = 0; g < N; g++) {
            for (int y = Integer.toString(g).length() - 1; y < maxLength; y++) {
                System.out.print(" ");
            }
            System.out.print(g);
        }
        System.out.println();
        for (int i = 0; i < M; i++) {
            for (int y = Integer.toString(i).length(); y < Integer.toString(M).length(); y++) {
                System.out.print(" ");
            }
            System.out.print(i);
            for (int j = 0; j < N; j++) {
                for (int h = 0; h < maxLength; h++) {
                    System.out.print(" ");
                }
                System.out.print(this.board[i][j]);
            }
            System.out.println();
        }
    }
}
