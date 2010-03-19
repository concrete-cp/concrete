package cspfj.constraint;

import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {

    protected int[][][] last;

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
                && controlTuplePresence(last[variablePosition][index],
                        variablePosition)) {
            return true;
        }
        if (findSupport(variablePosition, index)) {
            updateResidues();
            return true;
        }
        return false;
    }

    protected boolean findSupport(final int variablePosition, final int index) {
        tupleManager.setFirstTuple(variablePosition, index);

        do {
            if (chk()) {
                return true;
            }
        } while (tupleManager.setNextTuple(variablePosition));

        return false;
    }

    protected void updateResidues() {
        if (last != null) {
            final int[] residue = tuple.clone();
            for (int position = getArity(); --position >= 0;) {
                last[position][residue[position]] = residue;
            }
        }
    }

    public void removeTupleCache() {
        last = null;
    }
}
