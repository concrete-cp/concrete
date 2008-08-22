package cspfj.filter;

import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Deque;
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
public final class AC3CF implements Filter {
	private final Problem problem;

	private final Deque<Constraint> queue;

	// private int queueSize = 0;

	private final boolean[] cstInQueue;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3CF(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		cstInQueue = new boolean[problem.getMaxCId() + 1];

		queue = new ArrayDeque<Constraint>(problem.getNbConstraints());

		// queue = new ArrayDeque<Constraint>();
	}

	public boolean reduceAll(final int level) {
		queue.clear();
		queue.addAll(problem.getConstraintsAsCollection());
		Arrays.fill(cstInQueue, true);

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}

		return reduce(level);
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		while(!queue.isEmpty()) {
			queue.pollFirst().fillRemovals(false);
		}
		Arrays.fill(cstInQueue, false);

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int i = involving.length; --i >= 0;) {
			final Constraint c = involving[i];
			cstInQueue[c.getId()] = true;
			queue.add(c);
			c.setRemovals(variable.getPositionInConstraint(i), true);
		}
		return reduce(level);
	}

	private RevisionHandler revisionHandler = new RevisionHandler() {

		@Override
		public void revised(Constraint constraint, Variable variable) {
			final Constraint[] involving = variable.getInvolvingConstraints();

			for (int i = involving.length; --i >= 0;) {
				final Constraint c = involving[i];

				if (c != constraint) {
					if (!cstInQueue[c.getId()]) {
						cstInQueue[c.getId()] = true;
						queue.add(c);
					}
					c.setRemovals(variable.getPositionInConstraint(i), true);
				}
			}
		}

	};

	private boolean reduce(final int level) {
		logger.fine("Reducing");

		final RevisionHandler revisionHandler = this.revisionHandler;
		final Deque<Constraint> queue = this.queue;

		while (!queue.isEmpty()) {
			Constraint constraint = queue.pollFirst();

			cstInQueue[constraint.getId()] = false;

			if (!constraint.revise(level, revisionHandler)) {
				constraint.incWeight();
				return false;
			}
			
			constraint.fillRemovals(false);

		}

		return true;

	}

	public String toString() {
		return "GAC3rm-constraint";
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
