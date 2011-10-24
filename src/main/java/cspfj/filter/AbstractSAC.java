package cspfj.filter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import scala.collection.IndexedSeq;
import cspfj.StatisticsManager;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Statistic;

abstract class AbstractSAC implements Filter {

    protected final AC3 filter;

    protected final Problem problem;

    @Statistic
    protected int nbSingletonTests = 0;

    // private final Comparator<Variable> heuristic;

    private static final Logger LOGGER = Logger.getLogger(AbstractSAC.class
            .toString());

    static {
        StatisticsManager.register(AbstractSAC.class);
    }

    public AbstractSAC(final Problem problem, final AC3 filter) {
        super();
        this.filter = filter;
        this.problem = problem;
        // heuristic = new Dom(problem);
    }

    public boolean reduceAfter(final Variable variable) {
        if (variable == null) {
            return true;
        }
        try {
            return reduceAll();
        } catch (InterruptedException e) {
            throw new IllegalArgumentException(
                    "Filter was unexpectingly interrupted !");
        }
    }

    protected abstract boolean singletonTest(Variable variable)
            throws InterruptedException;

    protected boolean reduce() throws InterruptedException {
        final Filter filter = this.filter;

        if (!filter.reduceAll()) {
            return false;
        }
        final IndexedSeq<Variable> variables = problem.variables();

        int mark = 0;

        int v = 0;

        do {
            final Variable variable = variables.apply(v);
                    // if (logger.isLoggable(Level.FINE)) {
            LOGGER.info(variable.toString());
            // }
            if (variable.dom().size() > 1 && singletonTest(variable)) {
                if (variable.dom().size() <= 0) {
                    return false;
                }
                if (!filter.reduceAfter(variable)) {
                    return false;
                }
                mark = v;
            }
            if (++v >= variables.size()) {
                v = 0;
            }
        } while (v != mark);

        return true;

    }

    @Override
    public boolean reduceAfter(Collection<Constraint> constraints) {
        throw new UnsupportedOperationException();
    }

    public boolean reduceAll() throws InterruptedException {
        return reduce();
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>();
        statistics.put("SAC-nbsingletontests", nbSingletonTests);
        for (Entry<String, Object> stat : filter.getStatistics().entrySet()) {
            statistics.put("SAC-backend-" + stat.getKey(), stat.getValue());
        }
        return statistics;
    }

}