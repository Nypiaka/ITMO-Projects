package game;

import java.util.Random;

public class BotPlayer extends Player {
    private final Random random = new Random();

    @Override
    public int firststep() {
        return 1;
    }

    @Override
    public int xStep(int M) {
        return (int) (Math.random() * M);
    }

    @Override
    public int yStep(int N) {
        return (int) (Math.random() * N);
    }

    @Override
    public int drawChoice() {
        return random.nextInt(2) + 1;
    }

    @Override
    public GameResult step(int m, int n, int x, int y) {
        if (!Game.drawLoger) {
            return GameResult.STEP;
        } else {
            int drawRes = drawChoice();
            if (drawRes == 1) {
                return GameResult.DRAW;
            }
            Game.drawLoger = false;
            return GameResult.UNKNOWN;
        }
    }
}

