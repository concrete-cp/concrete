package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Neq extends AbstractConstraint {

    /**
     * Corresponding scope is reversed!
     */
    private final int[][] corresponding;

    public Neq(final Variable v0, final Variable v1) {
        super(v0, v1);
        final Domain v0Dom = v0.getDomain();
        final Domain v1Dom = v1.getDomain();
        corresponding = new int[][] { new int[v1Dom.maxSize()],
                new int[v0Dom.maxSize()] };

        for (int i = v0Dom.first(); i >= 0; i = v0Dom.next(i)) {
            corresponding[1][i] = v1Dom.index(v0Dom.value(i));
        }
        for (int i = v1Dom.first(); i >= 0; i = v1Dom.next(i)) {
            corresponding[0][i] = v0Dom.index(v1Dom.value(i));
        }
    }

    @Override
    public boolean check() {
        return getValue(0) != getValue(1);
    }

    @Override
    public boolean revise(final RevisionHandler revisionHandler,
            final int revisionCount) {
        for (int p = 2; --p >= 0;) {
            final Domain dom = getVariable(p).getDomain();
            if (dom.size() == 1) {
                final Variable otherVar = getVariable(1 - p);
                final int index = corresponding[1 - p][dom.first()];
                if (index >= 0 && otherVar.isPresent(index)) {
                    if (otherVar.getDomainSize() == 1) {
                        return false;
                    }
                    otherVar.remove(index);
                    revisionHandler.revised(this, getVariable(1 - p));
                    entail();
                }
            }
        }
        return true;
    }

    public String toString() {
        return getVariable(0) + " /= " + getVariable(1);
    }

    @Override
    public float getEvaluation() {
        return getArity();
    }

}
