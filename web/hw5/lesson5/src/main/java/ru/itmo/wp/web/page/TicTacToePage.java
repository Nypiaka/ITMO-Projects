package ru.itmo.wp.web.page;


import javax.servlet.http.HttpServletRequest;
import java.util.Map;


@SuppressWarnings({"unused", "RedundantSuppression"})
public class TicTacToePage {
    private void action(Map<String, Object> view, HttpServletRequest request) {
        newGame(view, request);

    }

    private void newGame(Map<String, Object> view, HttpServletRequest request) {
        State state = new State();
        view.put("state", state);
        request.getSession().setAttribute("state", state);
    }

    private void onMove(HttpServletRequest request, Map<String, Object> view) {
        for (Map.Entry<String, String[]> parameter : request.getParameterMap().entrySet()) {
            State state = (State) request.getSession().getAttribute("state");
            if (checkParameter(parameter, 2)) {
                if (state.getPhase().equals("RUNNING")) {
                    state.doMove(Character.getNumericValue(parameter.getKey().charAt(5)),
                            Character.getNumericValue(parameter.getKey().charAt(6)));
                    break;
                }
            }
            request.getSession().setAttribute("state", state);
            view.put("state", state);
        }
    }

    private boolean checkParameter(Map.Entry<String, String[]> parameter, int size) {
        boolean result = parameter.getKey().startsWith("cell_") && (parameter.getKey().length() == 5 + size);
        for (int i = 5; i < 5 + size; i++) {
            result = result && Character.isDigit(parameter.getKey().charAt(i));
        }
        return result;
    }


    public static class State {
        Map<Integer, String> markerOfCurrentPlayer = Map.of(0, "X", 1, "O");
        private final int size;
        private final int numberOfPlayersMarkersToWin;
        private final int numberOfPlayers;
        private int numberOfCurrentPlayer;
        private int numberOfSteps;
        private String[][] board;
        private String phase;

        public State() {
            this.size = 3;
            this.board = new String[size][size];
            this.phase = "RUNNING";
            this.numberOfCurrentPlayer = 0;
            this.numberOfSteps = 0;
            this.numberOfPlayers = 2;
            this.numberOfPlayersMarkersToWin = 3;
        }

        public int getSize() {
            return size;
        }

        public String getPhase() {
            return phase;
        }

        public String[][] getCells() {
            return board;
        }

        public boolean isCrossesMove() {
            return numberOfCurrentPlayer == 0;
        }

        public void doMove(int row, int column) {
            if (0 <= row && row < size && 0 <= column && column < size && board[row][column] == null) {
                board[row][column] = markerOfCurrentPlayer.get(numberOfCurrentPlayer);
                numberOfSteps++;
                checkCurrentPlayersWinning(row, column);
                checkDraw();
                numberOfCurrentPlayer = (numberOfCurrentPlayer + 1) % (numberOfPlayers);
            }
        }

        private void checkDraw() {
            if (numberOfSteps == size * size) {
                phase = "DRAW";
            }
        }

        private void checkCurrentPlayersWinning(int row, int column) {
            checkCurrentPlayersWinningByStepsType(-1, 1, row, column);
            checkCurrentPlayersWinningByStepsType(-1, -1, row, column);
            checkCurrentPlayersWinningByStepsType(0, 1, row, column);
            checkCurrentPlayersWinningByStepsType(1, 0, row, column);
        }

        private void checkCurrentPlayersWinningByStepsType(int stepByRow, int stepByColumn, int row, int column) {
            int currentRow = row + stepByRow;
            int currentColumn = column + stepByColumn;
            int numberOfConsecutiveCurrentPlayersMarkers =
                    checkCurrentPlayersWinningByCurrentStepsTypeByCourse(stepByRow, stepByColumn, row, column, 1)
                            + checkCurrentPlayersWinningByCurrentStepsTypeByCourse(stepByRow, stepByColumn, row, column, -1) + 1;
            if (numberOfConsecutiveCurrentPlayersMarkers == numberOfPlayersMarkersToWin) {
                phase = "WON_" + markerOfCurrentPlayer.get(numberOfCurrentPlayer);
            }
        }

        private int checkCurrentPlayersWinningByCurrentStepsTypeByCourse(int stepByRow, int stepByColumn, int row, int column, int course) {
            int currentRow = row + course * stepByRow;
            int currentColumn = column + course * stepByColumn;
            int numberOfConsecutiveCurrentPlayersMarkers = 0;
            while (currentRow >= 0 && currentRow < size && currentColumn >= 0 &&
                    currentColumn < size && board[currentRow][currentColumn] != null &&
                    board[currentRow][currentColumn].equals(markerOfCurrentPlayer.get(numberOfCurrentPlayer))) {
                numberOfConsecutiveCurrentPlayersMarkers++;
                currentRow += course * stepByRow;
                currentColumn += course * stepByColumn;
            }
            return numberOfConsecutiveCurrentPlayersMarkers;
        }
    }
}