package cspfj.constraint.semantic;

import java.util.Arrays;

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.generator.ConstraintManager;
import cspfj.problem.BitVectorDomain;
import cspfj.problem.Domain;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.constraint.CSPOMConstraint;
import cspom.constraint.FunctionalConstraint;
import cspom.constraint.GeneralConstraint;
import cspom.variable.CSPOMVariable;

public final class Disj extends AbstractConstraint {

    static {
        ConstraintManager.register("or", Disj.class);
    }

    private int watch1 = -1;
    private int watch2 = -1;
    private final boolean[] reverses;

    public Disj(Variable... disj) {
        this(disj, new boolean[disj.length]);
    }

    public Disj(Variable[] disj, boolean[] reverses) {
        super(disj);
        for (Variable v : disj) {
            if (v.getDomainSize() != 2
                    || !Arrays.equals(v.getDomain().allValues(), new int[] { 0,
                            1 })) {
                throw new IllegalArgumentException(
                        "Assigning constants or non-booleans to disjunctions is not allowed");
            }
        }
        this.reverses = reverses;
        watch1 = seekWatch(-1);
        if (watch1 < 0) {
            throw new IllegalStateException("Unexpected inconsistency");
        }
        watch2 = seekWatch(watch1);
        if (watch2 < 0) {
            throw new IllegalStateException("Unexpected inconsistency");
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
    public boolean revise(RevisionHandler revisator, int reviseCount) {
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
                    return false;
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

    private int seekWatch(int excluding) {
        for (int i = 0; i < getArity(); i++) {
            if (i != excluding && canBeTrue(i)) {
                return i;
            }
        }
        return -1;
    }

    public int getEvaluation(int rev) {
        return getArity();
    }

    public static boolean generate(final CSPOMConstraint constraint,
            final Problem problem) {
        final Variable[] scope = ConstraintManager.getSolverVariables(
                constraint.getScope(), problem);

        for (Variable v : scope) {
            if (v.getDomain() == null) {
                v.setDomain(new BitVectorDomain(0, 1));
            }
        }
        
        if (constraint instanceof GeneralConstraint) {

            problem.addConstraint(new Disj(scope));
            return true;

        } else if (constraint instanceof FunctionalConstraint) {

            /*
             * Reified disjunction is converted to CNF :
             * 
             * a = b v c v d...
             * 
             * <=>
             * 
             * (-a v b v c v d...) ^ (a v -b) ^ (a v -c) ^ (a v -d) ^ ...
             */
            final boolean[] reverses = new boolean[scope.length];
            reverses[0] = true;
            problem.addConstraint(new Disj(scope, reverses));

            final FunctionalConstraint fConstraint = (FunctionalConstraint) constraint;
            final Variable result = problem.getSolverVariable(fConstraint
                    .getResultVariable());

            for (CSPOMVariable v : fConstraint.getArguments()) {
                problem.addConstraint(new Disj(new Variable[] { result,
                        problem.getSolverVariable(v) }, new boolean[] { false,
                        true }));
            }

            return true;

        } else {
            throw new IllegalArgumentException("Unhandled constraint type for "
                    + constraint);
        }
    }

}
