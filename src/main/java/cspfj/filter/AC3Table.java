package cspfj.filter;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.heuristic.VariableHeuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3Table implements Filter {
	private final Problem problem;

	private final boolean[] inQueue;

	private int queueSize;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3Table(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new boolean[problem.getMaxVId() + 1];
		queueSize = 0;
	}

	public boolean reduceAll(final int level) {
		addAll();
		return reduce(level);

	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		for (int i = inQueue.length; --i >= 0;) {
			if (inQueue[i]) {
				for (Constraint c : problem.getVariable(i)
						.getInvolvingConstraints()) {
					c.fillRemovals(false);
				}
			}
		}
		Arrays.fill(inQueue, false);

		inQueue[variable.getId()] = true;
		queueSize = 1;

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			involving[cp].setRemovals(variable.getPositionInConstraint(cp),
					true);

		}

		return reduce(level);
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {
			if (!inQueue[variable.getId()]) {
				inQueue[variable.getId()] = true;
				queueSize++;
			}
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

	private boolean reduce(final int level) {
		logger.finer("Reducing");

		while (queueSize > 0) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {
				final Constraint constraint = constraints[c];

				final int position = variable.getPositionInConstraint(c);

				if (!constraint.getRemovals(position)) {
					constraint.fillRemovals(false);
					continue;
				}

				if (!constraint.revise(level, revisator)) {
					effectiveRevisions++;
					constraint.incWeight();
					constraint.fillRemovals(false);
					return false;
				}

				constraint.fillRemovals(false);

			}

		}

		return true;

	}

	private void addAll() {
		Arrays.fill(inQueue, false);
		for (Variable v : problem.getVariables()) {
			inQueue[v.getId()] = true;
		}
		queueSize = problem.getNbVariables();

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}
	}

	private Variable pullVariable() {
		Variable bestVariable = null;
		int bestValue = Integer.MAX_VALUE;

		final boolean[] inQueue = this.inQueue;

		final Problem problem = this.problem;

		for (int i = inQueue.length; --i >= 0;) {
			if (inQueue[i]) {
				final Variable variable = problem.getVariable(i);
				final int domainSize = variable.getDomainSize();
				if (domainSize < bestValue) {
					bestVariable = variable;
					bestValue = domainSize;
				}
			}
		}

		inQueue[bestVariable.getId()] = false;
		queueSize--;
		return bestVariable;
	}

	public String toString() {
		return "GAC3rm";
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("gac3-effective-revisions", effectiveRevisions);
		statistics.put("gac3-useless-revisions", uselessRevisions);
		return statistics;
	}


	@Override
	public boolean ensureAC() {
		return true;
	}
}
