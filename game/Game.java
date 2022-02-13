package game;

public class Game implements CharTaker, Error {
    private final Interpreter gameInterpreter = new Interpreter();
    private Player player1;
    private Player player2;
    private int M;
    private int N;
    private int K;
    public static boolean drawLoger;
    private int no = 2;
    private final StepChecker stepChecker = new StepChecker();
    private int x = -1;
    private int y = -1;
    private int res = -1;

    public int TicTacGame() {
        drawLoger = false;
        AbstractBoard currentBoard = makeBoard();
        makePlayers();
        while (true) {
            printBoundaries();
            no = 1;
            step(currentBoard, player1);
            if (res != -1) return res;
            printBoundaries();
            no = 2;
            step(currentBoard, player2);
            if (res != -1) return res;
        }
    }

    private void step(AbstractBoard currentBoard, Player currentPlayer) {
        System.out.println("Ход игрока " + no);
        currentBoard.printBoard();
        GameResult result = currentPlayer.step(M, N, x, y);
        if (result == GameResult.STEP) {
            GameResult b = stepChecker.checkStep(currentBoard, x, y, currentPlayer, K);
            if (b != GameResult.UNKNOWN) {
                if (b == GameResult.WIN) res = no;
                else if (b == GameResult.DRAW) res = 0;
                else res = 3 - no;
            }
        } else if (result != GameResult.UNKNOWN && result != GameResult.WIN) {
            if (result == GameResult.LOSE) res = 3 - no;
            else res = 0;
        }
    }

    private void printBoundaries() {
        System.out.println();
        for (int k = 0; k < 100; k++) {
            System.out.print("==");
        }
        System.out.println();
    }

    private AbstractBoard makeBoard() {
        System.out.println("""
                Введите тип поля, на котором вы хотели бы сыграть:
                1: прямоугольное поле
                2: гекс""");
        int res = gameInterpreter.interpreter();
        while (!(res == 1 || res == 2)) {
            System.out.println(ERROR);
            res = gameInterpreter.interpreter();
        }
        AbstractBoard board1;
        if (res == 1) {
            board1 = new BoardClassic();
        } else board1 = new BoardHex();
        board1.full(M, N);
        M = board1.board().length;
        N = board1.board()[0].length;
        System.out.println("Введите длину цепочки.");
        K = gameInterpreter.interpreter();
        while (K <= 0) {
            System.out.println(ERROR);
            K = gameInterpreter.interpreter();
        }
        return board1;
    }

    private void makePlayers() {
        for (int i = 1; i < 3; i++) {
            System.out.println("Введите тип " + i + " игрока:" + '\n' +
                    "1: человек" + '\n' +
                    "2: бот");
            int res = gameInterpreter.interpreter();
            while (!(res == 1 || res == 2)) {
                System.out.println(ERROR);
                res = gameInterpreter.interpreter();
            }
            if (res == 1) {
                if (i == 1) {
                    player1 = new HumanPlayer();
                    player1.symb = CharTaker.firstPlayersMark;
                } else {
                    player2 = new HumanPlayer();
                    player2.symb = CharTaker.secondPlayersMark;
                }
            } else if (i == 1) {
                player1 = new BotPlayer();
                player1.symb = CharTaker.firstPlayersMark;
            } else {
                player2 = new BotPlayer();
                player2.symb = CharTaker.secondPlayersMark;
            }
        }
    }
}
