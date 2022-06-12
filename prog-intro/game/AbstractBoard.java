package game;

public abstract class AbstractBoard implements CharTaker {
    protected char[][] board;
    protected int drawSize;

    protected boolean checker(int x, int y) {
        return !(x >= 0 && y >= 0 && x < this.board.length && y < this.board[0].length && this.board[x][y] == CharEmpty);
    }

    protected void printBoard() {
    }

    protected boolean checkWin(int a, int b, int k) {
        return false;
    }

    protected void full(int m, int n) {
    }

    protected char[][] board() {
        return null;
    }

    protected boolean easyCheck(int x, int y, int plusX, int plusY, int K) {
        int count = 0;
        int x1 = x;
        int y1 = y;
        int granM;
        int granN;
        if (plusX > 0) {
            granM = board.length;
        } else {
            granM = -1;
        }
        if (plusY > 0) {
            granN = board[0].length;
        } else {
            granN = -1;
        }
        while (x1 != granM && y1 != granN && board[x1][y1] == board[x][y]) {
            count++;
            x1 += plusX;
            y1 += plusY;
        }
        if (count >= K) {
            return true;
        } else {
            if (plusX > 0) {
                granM = -1;
            } else {
                granM = board.length;
            }
            if (plusY > 0) {
                granN = -1;
            } else {
                granN = board[0].length;
            }
            x1 = x;
            y1 = y;
            count--;
            while (x1 != granM && y1 != granN && board[x1][y1] == board[x][y]) {
                count++;
                x1 -= plusX;
                y1 -= plusY;
            }
            return count >= K;
        }
    }

}
