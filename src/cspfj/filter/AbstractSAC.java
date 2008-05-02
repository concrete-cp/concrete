/*
 * Created on 4 déc. 07
 *
 * To change the template for this generated file go to
 * Window&gt;Preferences&gt;Java&gt;Code Generation&gt;Code and Comments
 */
package cspfj.filter;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.heuristic.Dom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public abstract class AbstractSAC implements BackedFilter {

	protected final Filter filter;

	protected final Problem problem;

	protected int nbSingletonTests = 0;

	private final Variable[] variables;

	private final Comparator<Variable> heuristic;

	private static final Logger logger = Logger.getLogger(AbstractSAC.class
			.toString());

	public AbstractSAC(Problem problem, Filter filter) {
		super();
		this.filter = filter;
		this.problem = problem;
		this.variables = problem.getVariables().clone();
		heuristic = new Dom(problem);
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		try {
			return reduceAll(level);
		} catch (InterruptedException e) {
			throw new IllegalArgumentException(
					"Filter was unexpectingly interrupted !");
		}
	}

	protected abstract boolean singletonTest(Variable variable, int level)
			throws InterruptedException;

	protected boolean check(final Variable variable, final int index,
			final int level) {
		if (logger.isLoggable(Level.FINEST)) {
			logger.finest(level + " : " + variable + " <- "
					+ variable.getDomain()[index] + "(" + index + ")");
		}

		variable.assign(index, problem);
		problem.setLevelVariables(level, variable);
		nbSingletonTests++;
		final boolean singletonTest = filter.reduceAfter(level + 1, variable);
		variable.unassign(problem);
		problem.restore(level + 1);

		if (!singletonTest) {
			logger.finer("Removing " + variable + ", " + index);

			variable.remove(index, level);
			return true;
		}

		return false;
	}

	protected boolean reduce(final int level) throws InterruptedException {
		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}
		final Variable[] variables = this.variables;
		Arrays.sort(variables, heuristic);
		// System.out.println(Arrays.toString(variables));
		// // Tri à bulles
		// for (int i = 0; i < variables.length; i++) {
		// boolean changed = false;
		// for (int j = variables.length; --j > i;) {
		// if (variables[j].getDomainSize() < variables[j - 1]
		// .getDomainSize()) {
		// final Variable temp = variables[j];
		// variables[j] = variables[j - 1];
		// variables[j - 1] = temp;
		// changed = true;
		// }
		// }
		// if (!changed) {
		// break;
		// }
		// }

		int mark = 0;

		int v = 0;

		do {
			final Variable variable = variables[v];
			if (logger.isLoggable(Level.FINE)) {
				logger.fine(variable.toString());
			}
			if (variable.getDomainSize() > 1 && singletonTest(variable, level)) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}
				if (!filter.reduceAfter(level, variable)) {
					return false;
				}
				// mark = v;
			}

			v = next(v, variables.length);
		} while (v != mark);

		return true;

	}

	public boolean reduceAll(final int level) throws InterruptedException {
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
		logger.fine("Tour !");
		return 0;
	}

	@Override
	public boolean ensureAC() {
		return filter.ensureAC();
	}

}