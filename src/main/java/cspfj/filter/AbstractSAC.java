package cspfj.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

abstract class AbstractSAC implements Filter {

    protected final AC3 filter;

    protected final Problem problem;

    protected int nbSingletonTests = 0;

    // private final Comparator<Variable> heuristic;

    private static final Logger LOGGER = Logger.getLogger(AbstractSAC.class
            .toString());

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
        final Variable[] variables = problem.getVariables();

        int mark = 0;

        int v = 0;

        do {
            final Variable variable = variables[v];
            // if (logger.isLoggable(Level.FINE)) {
            LOGGER.info(variable.toString());
            // }
            if (variable.getDomainSize() > 1 && singletonTest(variable)) {
                if (variable.getDomainSize() <= 0) {
                    return false;
                }
                if (!filter.reduceAfter(variable)) {
                    return false;
                }
                mark = v;
            }
            if (++v >= variables.length) {
                v = 0;
            }
        } while (v != mark);

        return true;

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