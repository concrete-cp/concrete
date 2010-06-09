package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class InInterval extends AbstractConstraint {

    private final int lb, ub;
    private final Domain domain;

    public InInterval(final Variable variable, final int lb, final int ub) {
        super(variable);

        this.domain = variable.getDomain();
        this.lb = domain.lowest(lb);
        this.ub = domain.greatest(ub);
    }

    @Override
    public int getEvaluation(final int reviseCount) {
        return 0;
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        final int removed = domain.removeTo(lb - 1) + domain.removeFrom(ub + 1);
        if (removed > 0) {
            if (domain.size() <= 0) {
                return false;
            }
            revisator.revised(this, getVariable(0));
        }
        entail();
        return true;
    }

    @Override
    public boolean isConsistent(final int reviseCount) {
        for (int i = lb; i <= ub; i = domain.next(i)) {
            if (domain.present(i)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean check() {
        final int value = getValue(0);
        return lb <= value && value <= ub;
    }

    @Override
    public String toString() {
        return getVariable(0) + " in [" + getVariable(0).getValue(lb) + ", "
                + getVariable(0).getValue(ub) + "]";
    }
}
