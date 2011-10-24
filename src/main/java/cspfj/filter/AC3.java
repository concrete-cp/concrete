package cspfj.filter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

import scala.collection.IndexedSeq;
import scala.collection.JavaConversions;
import cspfj.constraint.Constraint;
import cspfj.priorityqueues.BinomialHeap;
import cspfj.priorityqueues.Key;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {
    private final Problem problem;

    private final Queue<Variable> queue;

    // private static final Logger LOGGER = Logger.getLogger(Filter.class
    // .getSimpleName());

    private int revisions = 0;

    private int reviseCount = 0;

    public AC3(final Problem problem) {
        this(problem, new BinomialHeap<Variable>(new Key<Variable>() {
            @Override
            public float getKey(final Variable object) {
                return object.dom().size();
            }
        }));
    }

    public AC3(final Problem problem, final Queue<Variable> queue) {
        super();
        this.problem = problem;
        this.queue = queue;
    }

    public boolean reduceAll() {
        reviseCount++;
        queue.clear();
        for (Variable v : problem.getVariables()) {
            queue.offer(v);
        }

        for (Constraint c : JavaConversions.asJavaIterable(problem
                .constraints())) {
            c.fillRemovals(reviseCount);
        }
        return reduce();

    }

    public boolean reduceAfter(final Collection<Constraint> constraints) {
        reviseCount++;
        queue.clear();

        for (Constraint c : constraints) {
            c.fillRemovals(reviseCount);

            if (!c.revise(revisator, reviseCount)) {
                c.weight_$eq(c.weight() + 1);
                return false;
            }

            c.fillRemovals(-1);

        }

        return reduce();
    }

    public boolean reduceFrom(final int[] modVar, final int[] modCons,
            final int cnt) {
        reviseCount++;
        queue.clear();
        // LOGGER.fine("reduce after " + cnt);
        for (Variable v : problem.getVariables()) {
            if (modVar[v.getId()] > cnt) {
                queue.offer(v);
            }
        }

        if (modCons != null) {
            // final BitSet cons = new BitSet();
            for (Constraint c : JavaConversions.asJavaIterable(problem
                    .constraints())) {
                if (modCons[c.getId()] > cnt) {
                    // cons.set(c.getId());

                    c.fillRemovals(reviseCount);

                    if (!c.revise(revisator, reviseCount)) {
                        c.weight_$eq(c.weight() + 1);
                        return false;
                    }

                    c.fillRemovals(-1);

                }
            }

            // for (int i = modCons.length; --i >= 0;) {
            // assert modCons[i] <= cnt || cons.get(i);
            // }
        }

        return reduce();
    }

    public boolean reduceAfter(final Variable variable) {
        reviseCount++;
        if (variable == null) {
            return true;
        }
        queue.clear();

        queue.offer(variable);

        final IndexedSeq<Constraint> involving = variable.constraints();

        for (int cp = involving.size(); --cp >= 0;) {
            final Constraint constraint = involving.apply(cp);
            constraint.setRemovals(variable.positionInConstraint()[cp],
                    reviseCount);
        }

        return reduce();
    }

    private RevisionHandler revisator = new RevisionHandler() {
        public void revised(final Constraint constraint, final Variable variable) {
            queue.offer(variable);

            final IndexedSeq<Constraint> involvingConstraints = variable
                    .constraints();

            for (int cp = involvingConstraints.size(); --cp >= 0;) {
                final Constraint constraintP = involvingConstraints.apply(cp);
                if (constraintP != constraint) {
                    constraintP.setRemovals(
                            variable.positionInConstraint()[cp], reviseCount);
                }

            }
        }
    };

    private boolean reduce() {
        // LOGGER.finer("Reducing");

        while (!queue.isEmpty()) {
            if (!reduce(queue.poll())) {
                return false;
            }
        }

        assert control();

        return true;

    }

    private boolean reduce(final Variable variable) {
        final IndexedSeq<Constraint> involvingConstraints = variable
                .constraints();
        for (int c = involvingConstraints.size(); --c >= 0;) {
            final Constraint constraint = involvingConstraints.apply(c);
            if (constraint.isEntailed()
                    || constraint.hasNoRemovals(reviseCount)) {
                continue;
            }
            revisions++;
            if (!constraint.revise(revisator, reviseCount)) {
                constraint.weight_$eq(constraint.weight());
                return false;
            }

            constraint.fillRemovals(-1);
        }
        return true;
    }

    private boolean control() {
        // LOGGER.fine("Control");
        final RevisionHandler controlRevisator = new RevisionHandler() {

            @Override
            public void revised(final Constraint constraint,
                    final Variable variable) {
                assert false : constraint + ", " + variable;

            }

        };

        for (Constraint c : JavaConversions.asJavaIterable(problem.constraints())) {
            assert c.revise(controlRevisator, -1);
        }
        return true;
    }

    public String toString() {
        return "GAC3rm-var-" + queue.getClass().getSimpleName();
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>(1);
        statistics.put("revisions", revisions);
        return statistics;
    }

}
