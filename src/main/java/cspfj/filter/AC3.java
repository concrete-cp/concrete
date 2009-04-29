package cspfj.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {
    private final Problem problem;

    private final BitVector inQueue;

    private static final Logger logger = Logger.getLogger(Filter.class
            .getSimpleName());

    public AC3(final Problem problem) {
        super();
        this.problem = problem;

        inQueue = BitVector.factory(problem.getMaxVId() + 1, false);
    }

    public boolean reduceAll() {
        addAll();
        return reduce();

    }

    public boolean reduceFrom(int[] lastModified, int[] reviseMe, int cnt) {
        for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
                .nextSetBit(i + 1)) {
            final Variable v = problem.getVariable(i);
            final Constraint[] involved = v.getInvolvingConstraints();
            for (int j = involved.length; --j >= 0;) {
                involved[j].setRemovals(v.getPositionInConstraint(j), false);
            }
        }

        inQueue.fill(false);

        for (Variable v : problem.getVariables()) {
            if (lastModified[v.getId()] > cnt) {
                inQueue.set(v.getId());
                final Constraint[] involved = v.getInvolvingConstraints();
                for (int j = involved.length; --j >= 0;) {
                    involved[j].setRemovals(v.getPositionInConstraint(j), true);
                }
            }
        }

        if (reviseMe != null) {
            for (Constraint c : problem.getConstraints()) {
                if (reviseMe[c.getId()] > cnt) {
                    c.fillRemovals(true);

                    if (!c.revise(revisator)) {
                        c.incWeight();
                        c.fillRemovals(false);
                        return false;
                    }
                    c.fillRemovals(false);
                }
            }
        }

        return reduce();
    }

    public boolean reduceAfter(final Variable variable) {
        if (variable == null) {
            return true;
        }
        for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
                .nextSetBit(i + 1)) {
            final Variable v = problem.getVariable(i);
            final Constraint[] involved = v.getInvolvingConstraints();
            for (int j = involved.length; --j >= 0;) {
                involved[j].setRemovals(v.getPositionInConstraint(j), false);
            }
        }

        inQueue.fill(false);

        inQueue.set(variable.getId());

        final Constraint[] involving = variable.getInvolvingConstraints();

        for (int cp = involving.length; --cp >= 0;) {
            involving[cp].setRemovals(variable.getPositionInConstraint(cp),
                    true);

        }

        return reduce();
    }

    private RevisionHandler revisator = new RevisionHandler() {
        public void revised(final Constraint constraint, final Variable variable) {
            inQueue.set(variable.getId());
            final Constraint[] involvingConstraints = variable
                    .getInvolvingConstraints();

            for (int cp = involvingConstraints.length; --cp >= 0;) {
                final Constraint constraintP = involvingConstraints[cp];
                if (constraintP != constraint) {
                    constraintP.setRemovals(variable
                            .getPositionInConstraint(cp), true);
                }

            }
        }
    };

    private boolean reduce() {
        logger.finer("Reducing");
        final BitVector inQueue = this.inQueue;

        while (!inQueue.isEmpty()) {
            if (!reduceOnce(pullVariable())) {
                return false;
            }
        }

        assert control();

        return true;

    }

    public boolean reduceOnce(Variable variable) {

        final RevisionHandler revisator = this.revisator;
        final Constraint[] involvingConstraints = variable
                .getInvolvingConstraints();
        for (int c = involvingConstraints.length; --c >= 0;) {
            final Constraint constraint = involvingConstraints[c];
            if (constraint.hasNoRemovals()) {
                // constraint.fillRemovals(false);
                continue;
            }

            if (!constraint.revise(revisator)) {
                constraint.incWeight();
                constraint.fillRemovals(false);
                return false;
            }

            constraint.fillRemovals(false);

        }
        return true;
    }

    private void addAll() {
        for (Variable v : problem.getVariables()) {
            inQueue.set(v.getId());
        }

        for (Constraint c : problem.getConstraints()) {
            c.fillRemovals(true);
        }
    }

    private boolean revised;

    private boolean control() {

        final RevisionHandler revisator = new RevisionHandler() {

            @Override
            public void revised(Constraint constraint, Variable variable) {
                revised = true;

            }

        };

        revised = false;

        for (Constraint c : problem.getConstraints()) {
            if (c instanceof AbstractPVRConstraint) {
                for (int i = c.getArity(); --i >= 0;) {
                    if (!c.getVariable(i).isAssigned()) {
                        assert !((AbstractPVRConstraint) c).revise(i);
                    }
                }
            } else {
                assert c.revise(revisator);
                assert !revised;
            }

        }
        return true;
    }

    private Variable pullVariable() {
        Variable bestVariable = null;
        int bestValue = Integer.MAX_VALUE;

        final BitVector inQueue = this.inQueue;

        final Problem problem = this.problem;

        for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
                .nextSetBit(i + 1)) {
            final Variable variable = problem.getVariable(i);
            final int domainSize = variable.getDomainSize();
            if (domainSize < bestValue) {
                bestVariable = variable;
                bestValue = domainSize;
            }
        }

        inQueue.clear(bestVariable.getId());
        return bestVariable;
    }

    public String toString() {
        return "GAC3rm";
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>(0);
        return statistics;
    }

}
