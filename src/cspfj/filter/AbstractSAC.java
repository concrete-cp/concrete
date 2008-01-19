/*
 * Created on 4 déc. 07
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractSAC implements BackedFilter {

	protected final Filter filter;

	protected final Problem problem;

	protected int nbSingletonTests = 0;

	private static final Logger logger = Logger.getLogger(AbstractSAC.class
			.toString());

	public AbstractSAC(Problem problem, Filter filter) {
		super();
		this.filter = filter;
		this.problem = problem;
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		return reduceAll(level);
	}

	protected abstract boolean singletonTest(Variable variable, int level);

	protected boolean reduce(final int level) {
		final Problem problem = this.problem;

		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}

		final Variable[] variables = problem.getVariables();

		int mark = 0;

		int v = 0;

		do {
			final Variable variable = variables[v];

			if (singletonTest(variable, level)) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}
				if (!filter.reduceAfter(level, variable)) {
					return false;
				}
				mark = v;
			}

			v = next(v, variables.length);
		} while (v != mark);

		return true;

	}

	public boolean reduceAll(final int level) {
		return reduce(level);
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("SAC-nbsingletontests", nbSingletonTests);
		for (String key : filter.getStatistics().keySet()) {
			statistics.put("SAC-backend-" + key, filter.getStatistics()
					.get(key));
		}
		return statistics;
	}

	public static int next(final int i, final int length) {
		if (i < length - 1) {
			return i + 1;
		}
		logger.info("Tour !");
		return 0;
	}

}