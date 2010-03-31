package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {
    private static long checks = 0;

    public static final void clearStats() {
        checks = 0;
    }

    public static final long getChecks() {
        return checks;
    }

    private final int[][][] last;

    public AbstractAC3Constraint(Variable... scope) {
        this(null, scope);
    }

    public AbstractAC3Constraint(String name, Variable... scope) {
        super(name, scope);

        last = new int[getArity()][][];

        for (int i = getArity(); --i >= 0;) {
            last[i] = new int[getVariable(i).getDomain().maxSize()][];
        }
    }

    @Override
    public boolean revise(final int position) {
        final Variable variable = getVariable(position);

        assert !variable.isAssigned();

        boolean revised = false;

        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {

            if (!hasSupport(position, index)) {
                variable.remove(index);

                revised = true;
                setActive(true);

            }

        }

        return revised;
    }

    public boolean hasSupport(final int variablePosition, final int index) {
        assert this.isInvolved(getVariable(variablePosition));
        assert index >= 0;

        if (last[variablePosition][index] != null
                && controlTuplePresence(last[variablePosition][index])) {
            return true;
        }
        if (findSupport(variablePosition, index)) {
            updateResidues();
            return true;
        }
        return false;
    }

    public boolean findSupport(final int variablePosition, final int index) {
        tupleManager.setFirstTuple(variablePosition, index);

        do {
            checks++;
            if (check()) {
                return true;
            }
        } while (tupleManager.setNextTuple(variablePosition));

        return false;
    }

    public final void updateResidues() {
        final int[] residue = tuple.clone();
        for (int position = getArity(); --position >= 0;) {
            last[position][residue[position]] = residue;
        }
    }

}
