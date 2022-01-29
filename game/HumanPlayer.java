package game;

public class HumanPlayer extends Player implements CharTaker, Error {
    private final Interpreter humanPlayerInterpreter = new Interpreter();
    int drawCounter = 0;

    @Override
    public int drawCounter() {
        return drawCounter;
    }

    @Override
    public int firststep() {
        int u = humanPlayerInterpreter.interpreter();
        while (!(u == 1 || u == 2 || u == 3) || (u == 3 && this.drawCounter() == 1)) {
            System.out.println(ERROR);
            u = humanPlayerInterpreter.interpreter();
        }
        if (u == 3) {
            this.drawCounter = 1;
        }
        return u;
    }

    @Override
    public int xStep(int m) {
        System.out.println("Введите номер строки");
        return humanPlayerInterpreter.interpreter();
    }

    @Override
    public int yStep(int n) {
        System.out.println("Введите номер столбца");
        this.drawCounter = 0;
        return humanPlayerInterpreter.interpreter();
    }

    @Override
    public int drawChoice() {
        int u = humanPlayerInterpreter.interpreter();
        while (!(u == 1 || u == 2)) {
            System.out.println(ERROR);
            u = humanPlayerInterpreter.interpreter();
        }
        return u;
    }

    @Override
    public GameResult step(int m, int n, int x, int y) {
        if (!Game.drawLoger) {
            int choise;
            System.out.println("""
                    Выберите один из следующих вариантов:
                    1: ввести ход
                    2: сдаться
                    3: предложить ничью""");
            choise = firststep();
            if (choise == 1) {
                return GameResult.STEP;
            } else if (choise == 2) {
                return GameResult.LOSE;
            } else {
                drawCounter = 1;
                Game.drawLoger = true;
                return GameResult.UNKNOWN;
            }
        } else {
            System.out.println("Предложена ничья. Ответ противоположного игрока: " + '\n' + "1: согласиться" + '\n' + "2: отказаться");
            int drawRes = drawChoice();
            if (drawRes == 1) {
                return GameResult.DRAW;
            }
            Game.drawLoger = false;
        }
        return GameResult.UNKNOWN;
    }
}
