package expression;

import java.util.Objects;

public abstract class AbstractUnaryOperation implements Arithmetical {
    Arithmetical currentOperation;
    String leftPart;
    String rightPart;
    String miniPart;
    int priority;

    @Override
    public String toString() {
        return leftPart + this.currentOperation.toString() + rightPart;
    }

    @Override
    public int returnPriority() {
        return priority;
    }

    @Override
    public String toMiniString() {
        if (currentOperation.returnPriority() >= this.priority) {
            return miniPart + currentOperation.toMiniString();
        } else return leftPart + currentOperation.toMiniString() + rightPart;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.getClass(), currentOperation.hashCode());
    }

    @Override
    public boolean equals(Object object) {
        if (object != null && object.getClass() == getClass())
            return this.currentOperation == ((AbstractUnaryOperation) object).currentOperation;
        return false;

    }

}
