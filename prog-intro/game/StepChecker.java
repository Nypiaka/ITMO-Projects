package game;

public class StepChecker implements CharTaker, Error {
    HumanPlayer forCurrentHuman = new HumanPlayer();

    public GameResult checkStep(AbstractBoard currentBoard, int x, int y, Player player, int K) {
        while (currentBoard.checker(x, y)) {
            x = player.xStep(currentBoard.board.length);
            y = player.yStep(currentBoard.board[0].length);
            if (currentBoard.checker(x, y) && player.getClass().equals(forCurrentHuman.getClass()))
                System.out.println(ERROR);
            else if (!player.getClass().equals(forCurrentHuman.getClass()) && currentBoard.checker(x, y)) {
                System.out.println("Текущий бот ошибся");
                return GameResult.LOSE;
            }
        }
        currentBoard.board[x][y] = player.symb;
        if (currentBoard.checkWin(x, y, K)) {
            System.out.println();
            currentBoard.printBoard();
            return GameResult.WIN;
        }
        currentBoard.drawSize--;
        if (currentBoard.drawSize == 0) {
            System.out.println();
            currentBoard.printBoard();
            return GameResult.DRAW;
        }
        return GameResult.UNKNOWN;
    }
}
