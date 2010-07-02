package cspfj.constraint;

import cspfj.problem.Domain;
import cspfj.problem.Variable;

public abstract class AbstractAC3Constraint extends AbstractPVRConstraint {
    private static long checks = 0;

    public static final void clearStats() {
        checks = 0;
    }

    public static final long getChecks() {
        return checks;
    }

    private final ResidueManager last;

    public AbstractAC3Constraint(final Variable... scope) {
        this(null, scope);
    }

    public AbstractAC3Constraint(final String name, final Variable... scope) {
        super(name, scope);
        last = new ResidueManagerMap(getArity());
    }

    @Override
    public boolean revise(final int position) {
        final Variable var = getVariable(position);
        boolean revised = false;
        for (int index = var.getFirst(); index >= 0; index = var.getNext(index)) {

            final int[] residue = last.getResidue(position, index);
            if (residue != null && controlTuplePresence(residue)) {
                continue;
            }

            if (findSupport(position, index)) {
                last.updateResidue(tuple.clone());
            } else {
                var.remove(index);
                revised = true;
            }

        }

        return revised;
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

    @Override
    public float getEvaluation() {
        float size = 1;
        for (Variable v : getScope()) {
            size *= v.getDomainSize();
        }
        return size;
    }

}
