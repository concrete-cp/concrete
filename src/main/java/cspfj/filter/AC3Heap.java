package cspfj.filter;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.heuristic.VariableHeuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Heap;

/**
 * @author scand1sk
 * 
 */
public final class AC3Heap implements Filter {
	private final Problem problem;

	private final boolean[] inQueue;
	private Heap<Variable> queue;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3Heap(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new boolean[problem.getMaxVId() + 1];
		queue = new Heap<Variable>(new Comparator<Variable>() {
			@Override
			public int compare(Variable arg0, Variable arg1) {
				return arg0.getDomainSize() - arg1.getDomainSize();
			}
		}, new Variable[problem.getNbVariables()]);

	}

	public boolean reduceAll(final int level) {
		queue.clear();
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
				inQueue[i] = false;
			}
		}

		queue.clear();

		inQueue[variable.getId()] = true;
		queue.add(variable);
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
				queue.add(variable);
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
		logger.fine("Reducing");

		final Heap<Variable> queue = this.queue;
		final boolean[] inQueue = this.inQueue;

		while (!queue.isEmpty()) {
			final Variable variable = queue.pollFirst();
			inQueue[variable.getId()] = false;

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
		for (Variable v : problem.getVariables()) {
			inQueue[v.getId()] = true;
			queue.add(v);
		}

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}
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
