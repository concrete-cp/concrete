package cspfj.constraint.semantic;

import java.util.Arrays;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Disj extends AbstractConstraint {
    private int watch1 = -1;
    private int watch2 = -1;
    private final boolean[] reverses;

    public Disj(final Variable... disj) {
        this(disj, new boolean[disj.length]);
    }

    public Disj(final Variable[] disj, final boolean[] reverses) {
        super(disj);
        for (Variable v : disj) {
            if (v.getDomainSize() != 2
                    || !Arrays.equals(v.getDomain().allValues(), new int[] { 0,
                            1 })) {
                throw new IllegalArgumentException(
                        "Assigning constants or non-booleans to disjunctions is not allowed");
            }
        }
        if (reverses == null) {
            this.reverses = new boolean[getArity()];
        } else if (reverses.length != getArity()) {
            throw new IllegalArgumentException(
                    "reverses must cover all variables");
        } else {
            this.reverses = reverses;
        }
        watch1 = seekWatch(-1);
        if (watch1 < 0) {
            throw new IllegalStateException("Unexpected inconsistency");
        }
        watch2 = seekWatch(watch1);
        if (watch2 < 0) {
            setTrue(watch1);
            watch2 = watch1;
        }

    }

    @Override
    public boolean check() {
        for (int i = getArity(); --i >= 0;) {
            if (reverses[i] ^ getValue(i) == 1) {
                return true;
            }
        }
        return false;
    }

    public String toString() {
        return "\\/" + Arrays.toString(getScope());
    }

    @Override
    public boolean revise(final RevisionHandler revisator, final int reviseCount) {
        if (isFalse(watch1)) {
            final int newWatch = seekWatch(watch2);
            if (newWatch < 0) {
                if (!canBeTrue(watch2)) {
                    return false;
                }
                if (setTrue(watch2)) {
                    revisator.revised(this, getVariable(watch2));
                    return true;
                }
            } else {
                watch1 = newWatch;
            }
        }
        if (isFalse(watch2)) {
            final int newWatch = seekWatch(watch1);
            if (newWatch < 0) {
                if (!canBeTrue(watch1)) {
                    throw new IllegalStateException();
                }
                if (setTrue(watch1)) {
                    revisator.revised(this, getVariable(watch1));
                }
            } else {
                watch2 = newWatch;
            }
        }
        return true;
    }

    private boolean isFalse(final int position) {
        return reverses[position] ^ getVariable(position).isPresent(1) ^ true;
    }

    private boolean setTrue(final int position) {
        final Domain dom = getVariable(position).getDomain();
        if (dom.size() == 1) {
            return false;
        }
        if (reverses[position]) {
            dom.remove(1);
        } else {
            dom.remove(0);
        }
        return true;
    }

    private boolean canBeTrue(final int position) {
        if (reverses[position]) {
            return getVariable(position).isPresent(0);
        }
        return getVariable(position).isPresent(1);
    }

    private int seekWatch(final int excluding) {
        for (int i = 0; i < getArity(); i++) {
            if (i != excluding && canBeTrue(i)) {
                return i;
            }
        }
        return -1;
    }

    public int getEvaluation(final int rev) {
        return getArity();
    }

}
