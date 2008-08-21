package cspfj.filter;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Deque;
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
public final class AC3WSlowQueue implements Filter {
	private final Problem problem;

	private final boolean[] inQueue;
	private final Heap<Variable> varQueue;

	private final boolean[] cstInQueue;
	private final Deque<Constraint> slowQueue;

	// private int queueSize = 0;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3WSlowQueue(final Problem problem,
			final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new boolean[problem.getMaxVId() + 1];

		cstInQueue = new boolean[problem.getMaxCId() + 1];

		slowQueue = new ArrayDeque<Constraint>();
		varQueue = new Heap<Variable>(new Comparator<Variable>() {
			@Override
			public int compare(Variable arg0, Variable arg1) {
				return arg0.getDomainSize() - arg1.getDomainSize();
			}
		}, new Variable[problem.getNbVariables()]);

	}

	public boolean reduceAll(final int level) {
		clearQueue();
		addAll();
		return reduce(level);

	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		clearQueue();

		inQueue[variable.getId()] = true;
		varQueue.add(variable);

		return reduce(level);
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {
			if (!inQueue[variable.getId()]) {
				inQueue[variable.getId()] = true;
				varQueue.add(variable);
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

		while (!varQueue.isEmpty() || !slowQueue.isEmpty()) {
			if (varQueue.isEmpty()) {
				final Constraint constraint = slowQueue.pollFirst();
				cstInQueue[constraint.getId()] = false;

				if (!revise(constraint, level, true)) {
					constraint.incWeight();
					return false;
				}

				constraint.fillRemovals(false);

			} else {
				Variable variable = varQueue.pollFirst();
				inQueue[variable.getId()] = false;

				final Constraint[] constraints = variable
						.getInvolvingConstraints();

				for (int c = constraints.length; --c >= 0;) {
					final Constraint constraint = constraints[c];

					final int position = variable.getPositionInConstraint(c);
					if (!constraint.getRemovals(position)) {
						continue;
					}

					if (!revise(constraint, level, false)) {
						constraint.incWeight();
						return false;
					}

					constraint.fillRemovals(false);

				}
			}

		}

		return true;

	}

	private boolean revise(Constraint constraint, int level, final boolean slow) {
		if (!slow && constraint.isSlow()) {
			if (!cstInQueue[constraint.getId()]) {
				cstInQueue[constraint.getId()] = true;
				slowQueue.add(constraint);
			}
			return true;
		}
		return constraint.revise(level, revisator);
	}

	private void clearQueue() {
		varQueue.clear();
		Arrays.fill(inQueue, false);

		// queueSize = 0;
		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}
		slowQueue.clear();
		Arrays.fill(cstInQueue, false);
	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			inQueue[v.getId()] = true;
			varQueue.add(v);
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
