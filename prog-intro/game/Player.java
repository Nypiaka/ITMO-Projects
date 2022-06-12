package game;

abstract class Player {
    protected GameResult step(int m, int n, int x, int y) {
        return GameResult.UNKNOWN;
    }

    protected char symb;
    protected int drawCounter = 0;

    protected int xStep(int M) {
        return 0;
    }

    protected int yStep(int N) {
        return 0;
    }

    protected int firstStep() {
        return 0;
    }

    protected int drawChoice() {
        return 0;
    }

    protected int drawCounter() {
        return drawCounter;
    }

}
