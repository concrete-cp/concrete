package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractPVRConstraint extends AbstractConstraint {

    private boolean revised = false;

    public AbstractPVRConstraint(final Variable... scope) {
        super(scope);
        // removals = new boolean[getArity()];
    }

    public AbstractPVRConstraint(final String name, final Variable... scope) {
        super(name, scope);

    }

    /**
     * Try to filter values from variable getVariable(position).
     * 
     * @param position
     * @return true iff any value has been removed
     */
    public abstract boolean revise(final int position);

    @Override
    public final boolean revise(final RevisionHandler revisator, final int reviseCount) {
        for (int i = getArity(); --i >= 0;) {
            final Variable variable = getVariable(i);
            // assert (!variable.isAssigned() && skipRevision(i)) ? !revise(i)
            // : true : "Should not skip " + this + ", " + i;
            if (!variable.isAssigned() && !skipRevision(i, reviseCount)
                    && revise(i)) {
                if (variable.getDomainSize() <= 0) {
                    return false;
                }
                revisator.revised(this, variable);
            }
        }
        return true;
    }

    private boolean skipRevision(final int variablePosition,
            final int reviseCount) {
        // if (true)
        // return false;
        if (getArity() == 1) {
            if (revised) {
                return true;
            }
            revised = true;
            return false;
        }
        for (int y = getArity(); --y >= 0;) {
            if (y != variablePosition && getRemovals(y) >= reviseCount) {
                return false;
            }
        }

        return true;
    }
}
