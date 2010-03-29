package cspfj.constraint;

import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;

public abstract class AbstractFastPVRConstraint extends AbstractConstraint {

    private boolean revised = false;

    public AbstractFastPVRConstraint(final Variable... scope) {
        super(scope);
        // removals = new boolean[getArity()];
    }

    public AbstractFastPVRConstraint(final String name, final Variable... scope) {
        super(name, scope);

    }

    /**
     * Try to filter values from variable getVariable(position)
     * 
     * @param position
     * @return true iff any value has been removed
     */
    public abstract boolean revise(final int position);

    @Override
    public final boolean revise(final RevisionHandler revisator, int reviseCount) {
        for (int i = getArity(); --i >= 0;) {
            final Variable variable = getVariable(i);
            // assert (!variable.isAssigned() && skipRevision(i)) ? !revise(i)
            // : true : "Should not skip " + this + ", " + i;
            if (!variable.isAssigned() && revise(i)) {
                if (variable.getDomainSize() <= 0) {
                    return false;
                }
                revisator.revised(this, variable);
            }
        }
        return true;
    }

}