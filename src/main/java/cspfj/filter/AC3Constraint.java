package cspfj.filter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;

import scala.collection.IndexedSeq;
import scala.collection.JavaConversions;

import cspfj.ParameterManager;
import cspfj.StatisticsManager;
import cspfj.constraint.Constraint;
import cspfj.priorityqueues.BinomialHeap;
import cspfj.priorityqueues.Key;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Parameter;
import cspfj.util.Statistic;

/**
 * @author scand1sk
 * 
 */
public final class AC3Constraint implements Filter {

    private final Problem problem;

    private final Queue<Constraint> queue;

    // private static final Logger LOGGER = Logger.getLogger(Filter.class
    // .getSimpleName());

    @Statistic
    public int revisions = 0;

    private static int revisionCount = 0;

    @SuppressWarnings("rawtypes")
    @Parameter("ac.queue")
    private static Class<? extends Queue> queueType = BinomialHeap.class;

    @Parameter("ac.key")
    private static Key<Constraint> key = new Key<Constraint>() {
        @Override
        public double getKey(final Constraint object) {
            return object.getEvaluation();
        }

        @Override
        public String toString() {
            return "object.getEvaluation()";
        }
    };

    static {
        ParameterManager.register(AC3Constraint.class);
    }

    @SuppressWarnings("unchecked")
    public AC3Constraint(final Problem problem) {
        super();
        this.problem = problem;
        try {
            queue = queueType.getConstructor(Key.class).newInstance(key);
            StatisticsManager.register("ac.priorityQueue", queue);
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    public boolean reduceAll() {
        revisionCount++;
        queue.clear();
        addAll();
        return reduce();

    }

    public boolean reduceFrom(final int[] modVar, final int[] modCons,
            final int cnt) {
        revisionCount++;
        queue.clear();
        // LOGGER.fine("reduce after " + cnt);
        for (Variable v : problem.getVariables()) {
            if (modVar[v.getId()] > cnt) {
                final IndexedSeq<Constraint> involved = v.constraints();
                for (int j = involved.size(); --j >= 0;) {
                    final Constraint constraint = involved.apply(j);
                    if (!constraint.isEntailed()) {
                        constraint.setRemovals(v.positionInConstraint()[j],
                                revisionCount);

                        queue.offer(involved.apply(j));
                    }
                }
            }
        }

        if (modCons != null) {
            for (Constraint c : JavaConversions.asJavaIterable(problem
                    .constraints())) {
                if (modCons[c.getId()] > cnt && !c.isEntailed()) {
                    c.fillRemovals(revisionCount);

                    queue.offer(c);
                }
            }
        }

        return reduce();
    }

    public boolean reduceAfter(final Variable variable) {
        revisionCount++;
        if (variable == null) {
            return true;
        }
        queue.clear();

        final IndexedSeq<Constraint> involving = variable.constraints();

        for (int cp = involving.size(); --cp >= 0;) {
            if (!involving.apply(cp).isEntailed()) {
                involving.apply(cp).setRemovals(
                        variable.positionInConstraint()[cp], revisionCount);
                queue.offer(involving.apply(cp));
            }
        }

        return reduce();
    }

    private RevisionHandler revisator = new RevisionHandler() {
        public void revised(final Constraint constraint, final Variable variable) {
            final IndexedSeq<Constraint> involvingConstraints = variable
                    .constraints();

            for (int cp = involvingConstraints.size(); --cp >= 0;) {
                final Constraint constraintP = involvingConstraints.apply(cp);
                if (constraintP != constraint && !constraintP.isEntailed()) {
                    constraintP.setRemovals(
                            variable.positionInConstraint()[cp], revisionCount);
                    queue.offer(constraintP);
                }

            }
        }
    };

    private boolean reduce() {
        // LOGGER.finer("Reducing");
        while (!queue.isEmpty()) {
            final Constraint constraint = queue.poll();

            revisions++;
            if (!constraint.revise(revisator, revisionCount)) {
                constraint.weight_$eq(constraint.weight() + 1);
                return false;
            }

            constraint.fillRemovals(-1);
        }

        assert control();

        return true;

    }

    private void addAll() {
        for (Constraint c : JavaConversions.asJavaIterable(problem
                .constraints())) {
            if (!c.isEntailed()) {
                c.fillRemovals(revisionCount);
                queue.offer(c);
            }
        }
    }

    private boolean control() {

        final RevisionHandler controlRevisator = new RevisionHandler() {

            @Override
            public void revised(final Constraint constraint,
                    final Variable variable) {
                assert false;

            }

        };

        for (Constraint c : JavaConversions.asJavaIterable(problem
                .constraints())) {
            assert c.revise(controlRevisator, -1);
        }
        return true;
    }

    public String toString() {
        return "GAC3rm-cons+" + queue.getClass().getSimpleName();
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>(1);
        statistics.put("revisions", revisions);
        return statistics;
    }

    @Override
    public boolean reduceAfter(final Collection<Constraint> constraints) {
        revisionCount++;
        queue.clear();

        for (Constraint c : constraints) {
            c.fillRemovals(revisionCount);
            queue.offer(c);
        }

        return reduce();
    }

}
