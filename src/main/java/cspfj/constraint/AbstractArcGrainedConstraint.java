package cspfj.constraint;

import java.util.Arrays;

import cspfj.problem.Variable;

public abstract class AbstractArcGrainedConstraint extends AbstractConstraint {

    private final int[] removals;

    public AbstractArcGrainedConstraint(final Variable... scope) {
        super(scope);
        removals = new int[getArity()];
    }

    public AbstractArcGrainedConstraint(final String name, final Variable... scope) {
        super(name, scope);
        removals = new int[getArity()];
    }

    public final int getRemovals(final int position) {
        return removals[position];
    }

    public final void setRemovals(final int position, final int value) {
        removals[position] = value;
    }

    public final void fillRemovals(final int value) {
        Arrays.fill(removals, value);
    }

    public final boolean hasNoRemovals(final int value) {
        for (int i : removals) {
            if (i >= value) {
                return false;
            }
        }
        return true;
    }

}