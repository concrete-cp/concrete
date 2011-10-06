package cspfj.constraint.semantic;

import com.google.common.base.Preconditions;
import com.google.common.collect.ObjectArrays;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.BooleanDomain;
import cspfj.problem.Variable;

public final class ReifiedConstraint extends AbstractConstraint {

    private final BooleanDomain controlDomain;
    private final Constraint positiveConstraint;
    private final Constraint negativeConstraint;

    private RevisionHandler usualRevisator;
    private RevisionHandler usualReifiedRevisator;

    public ReifiedConstraint(final Variable controlVariable,
            final Constraint positiveConstraint,
            final Constraint negativeConstraint) {
        super(ObjectArrays.concat(controlVariable,
                positiveConstraint.getScope()));

        Preconditions.checkArgument(
                controlVariable.getDomain() instanceof BooleanDomain,
                "Control variable must be boolean");

        this.controlDomain = (BooleanDomain) controlVariable.getDomain();

        this.positiveConstraint = positiveConstraint;
        this.negativeConstraint = negativeConstraint;
        // positiveConstraint.fillRemovals(Integer.MAX_VALUE);
        // negativeConstraint.fillRemovals(Integer.MAX_VALUE);
    }

    private final class ReifiedRevisionHandler implements RevisionHandler {

        private final RevisionHandler reifiedRevisator;

        private ReifiedRevisionHandler(final RevisionHandler reifiedRevisator) {
            this.reifiedRevisator = reifiedRevisator;
        }

        @Override
        public void revised(final Constraint constraint, final Variable variable) {
            reifiedRevisator.revised(ReifiedConstraint.this, variable);
        }

    }

    @Override
    public void setLevel(final int level) {
        super.setLevel(level);
        positiveConstraint.setLevel(level);
        negativeConstraint.setLevel(level);
    }

    @Override
    public void restore(final int level) {
        super.restore(level);
        positiveConstraint.restore(level);
        negativeConstraint.restore(level);
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        if (controlDomain.isUnknown()) {
            if (!positiveConstraint.isConsistent(reviseCount)) {
                controlDomain.setFalse();
                if (noReifyRevise(negativeConstraint, revisator, reviseCount)) {
                    revisator.revised(this, getVariable(0));
                    return true;
                }
                return false;
            }

            if (!negativeConstraint.isConsistent(reviseCount)) {
                controlDomain.setTrue();
                if (noReifyRevise(positiveConstraint, revisator, reviseCount)) {
                    revisator.revised(this, getVariable(0));
                    return true;
                }
                return false;
            }
            return true;
        }
        if (controlDomain.isTrue())
            return noReifyRevise(positiveConstraint, revisator, reviseCount);
        if (controlDomain.isFalse())
            return noReifyRevise(negativeConstraint, revisator, reviseCount);

        throw new IllegalStateException();

    }

    private boolean noReifyRevise(final Constraint constraint,
            final RevisionHandler revisator, final int reviseCount) {
        final RevisionHandler reifiedRevisator;
        if (revisator == usualRevisator) {
            reifiedRevisator = usualReifiedRevisator;
        } else {
            reifiedRevisator = new ReifiedRevisionHandler(revisator);
            usualRevisator = revisator;
            usualReifiedRevisator = reifiedRevisator;
        }
        final int actualRevise;
        if (controlRemovals >= reviseCount) {
            actualRevise = -1;
        } else {
            actualRevise = reviseCount;
        }
        if (constraint.revise(reifiedRevisator, actualRevise)) {
            if (constraint.isEntailed()) {
                entail();
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean check() {
        System.arraycopy(tuple, 1, positiveConstraint.getTuple(), 0,
                positiveConstraint.getArity());

        return (getValue(0) == 1) == positiveConstraint.check();
    }

    @Override
    public float getEvaluation() {
        return positiveConstraint.getEvaluation()
                + negativeConstraint.getEvaluation();
    }

    @Override
    public String toString() {
        return getVariable(0) + " == (" + positiveConstraint + ")";
    }

    private int controlRemovals;

    @Override
    public void setRemovals(final int position, final int value) {
        if (position == 0) {
            controlRemovals = value;
        } else {
            positiveConstraint.setRemovals(position - 1, value);
            negativeConstraint.setRemovals(position - 1, value);
        }
    }

    @Override
    public void fillRemovals(final int value) {
        controlRemovals = value;
        positiveConstraint.fillRemovals(value);
        negativeConstraint.fillRemovals(value);
    }

    @Override
    public boolean hasNoRemovals(final int value) {
        return controlRemovals < value
                && positiveConstraint.hasNoRemovals(value);
    }

}
