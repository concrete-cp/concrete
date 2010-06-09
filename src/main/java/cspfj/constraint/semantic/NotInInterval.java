package cspfj.constraint.semantic;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class NotInInterval extends AbstractConstraint {

    private final int lb, ub;
    private final Domain domain;

    public NotInInterval(final Variable variable, final int lb, final int ub) {
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
        boolean changed = false;
        for (int i = lb; i <= ub; i = domain.next(i)) {
            if (domain.present(i)) {
                domain.remove(i);
                changed = true;
            }
        }
        if (changed) {
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
        return (this.lb > 0 && domain.first() < lb)
                || (this.ub >= 0 && domain.last() > ub);
    }

    @Override
    public boolean check() {
        final int value = getValue(0);
        return value < lb || ub < value;
    }

    @Override
    public String toString() {
        return getVariable(0) + " notin [" + getVariable(0).getValue(lb) + ", "
                + getVariable(0).getValue(ub) + "]";
    }
}
