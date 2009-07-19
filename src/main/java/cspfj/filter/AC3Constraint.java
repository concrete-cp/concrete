package cspfj.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.FibonacciHeap;
import cspfj.util.Key;

/**
 * @author scand1sk
 * 
 */
public final class AC3Constraint implements Filter {
    private final Problem problem;

    private final Queue<Constraint> inQueue;

    private static final Logger logger = Logger.getLogger(Filter.class
            .getSimpleName());

    private int revisions = 0;

    public AC3Constraint(final Problem problem) {
        super();
        this.problem = problem;

        inQueue = new FibonacciHeap<Constraint>(new Key<Constraint>() {
            @Override
            public int getKey(Constraint o1) {
                return o1.getEvaluation();
            }
        }, problem.getConstraints());
    }

    public boolean reduceAll() {
        clean();
        addAll();
        return reduce();

    }

    public boolean reduceFrom(int[] modVar, int[] modCons, int cnt) {
        clean();
        logger.fine("reduce after " + cnt);
        for (Variable v : problem.getVariables()) {
            if (modVar[v.getId()] > cnt) {
                final Constraint[] involved = v.getInvolvingConstraints();
                for (int j = involved.length; --j >= 0;) {
                    // involved[j].fillRemovals(true);
                    involved[j].setRemovals(v.getPositionInConstraint(j), true);
                    inQueue.offer(involved[j]);
                }
            }
        }

        if (modCons != null) {
            for (Constraint c : problem.getConstraints()) {
                if (modCons[c.getId()] > cnt) {
                    c.fillRemovals(true);
                    inQueue.offer(c);
                }
            }
        }

        return reduce();
    }

    /**
     * Cleans the variable queue and removals
     */
    private void clean() {
        // for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
        // .nextSetBit(i + 1)) {
        // final Variable v = problem.getVariable(i);
        // final Constraint[] involving = v.getInvolvingConstraints();
        // for (int j = involving.length; --j >= 0;) {
        // involving[j].setRemovals(v.getPositionInConstraint(j), false);
        // }
        // }

        for (Constraint c : problem.getConstraints()) {
            c.fillRemovals(false);
        }

        inQueue.clear();

        assert noRemovals();
    }

    private boolean noRemovals() {
        for (Constraint c : problem.getConstraints()) {
            if (!c.hasNoRemovals()) {
                return false;
            }
        }
        return true;
    }

    public boolean reduceAfter(final Variable variable) {
        if (variable == null) {
            return true;
        }
        clean();

        final Constraint[] involving = variable.getInvolvingConstraints();

        for (int cp = involving.length; --cp >= 0;) {
            involving[cp].setRemovals(variable.getPositionInConstraint(cp),
                    true);
            inQueue.offer(involving[cp]);

        }

        return reduce();
    }

    private RevisionHandler revisator = new RevisionHandler() {
        public void revised(final Constraint constraint, final Variable variable) {
            final Constraint[] involvingConstraints = variable
                    .getInvolvingConstraints();

            for (int cp = involvingConstraints.length; --cp >= 0;) {
                final Constraint constraintP = involvingConstraints[cp];
                if (constraintP != constraint) {
                    constraintP.setRemovals(variable
                            .getPositionInConstraint(cp), true);
                    inQueue.offer(constraintP);
                }

            }
        }
    };

    private boolean reduce() {
        logger.finer("Reducing");
        final RevisionHandler revisator = this.revisator;

        while (!inQueue.isEmpty()) {
            final Constraint constraint = inQueue.poll();

            revisions++;
            if (!constraint.revise(revisator)) {
                constraint.incWeight();
                constraint.fillRemovals(false);
                return false;
            }

            constraint.fillRemovals(false);
        }

        assert control();

        return true;

    }

    private void addAll() {
        for (Constraint c : problem.getConstraints()) {
            c.fillRemovals(true);
            inQueue.offer(c);
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
                        assert !((AbstractPVRConstraint) c).revise(i) : c
                                + ", " + c.getVariable(i);
                    }
                }
            } else {
                assert c.revise(revisator);
                assert !revised;
            }

        }
        return true;
    }

    public String toString() {
        return "GAC3rm";
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>(1);
        statistics.put("revisions", revisions);
        return statistics;
    }

}
