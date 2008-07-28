package cspfj.filter;

import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.heuristic.VariableHeuristic;
import cspfj.heuristic.WeightHeuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Heap;

/**
 * @author scand1sk
 * 
 */
public final class AC3Constraint implements Filter {
	private final Problem problem;

	private final Heap<Constraint> queue;

	// private int queueSize = 0;

	private final BitSet cstInQueue;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private final double[] sizes;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	private WeightHeuristic wvh;

	public AC3Constraint(final Problem problem,
			final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		cstInQueue = new BitSet(problem.getMaxCId() + 1);

		sizes = new double[problem.getMaxCId() + 1];

		queue = new Heap<Constraint>(new Comparator<Constraint>() {
			@Override
			public int compare(Constraint arg0, Constraint arg1) {
				return Double.compare(sizes[arg0.getId()], sizes[arg1.getId()]);
			}
		}, new Constraint[problem.getNbConstraints()]);
		// queue = new ArrayDeque<Constraint>();

		wvh = (heuristic instanceof WeightHeuristic) ? (WeightHeuristic) heuristic
				: null;
	}

	public boolean reduceAll(final int level) {
		clearQueue();
		addAll();
		return reduce(level);

	}

	private static double size(Constraint c) {
		double size = 1;
		for (int i = c.getArity(); --i >= 0;) {
			size *= c.getVariable(i).getDomainSize();
		}
		return size;
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		clearQueue();

		addInQueue(variable);

		return reduce(level);
	}

	private boolean reduce(final int level) {
		logger.fine("Reducing");

		final boolean[] revised = new boolean[problem.getMaxArity()];

		while (!queue.isEmpty()) {
			final Constraint constraint = pullConstraint();

			if (!constraint.revise(level, revised)) {
				if (wvh != null) {
					wvh.treatConflictConstraint(constraint);
				}
				return false;
			}

			for (int i = constraint.getArity(); --i >= 0;) {
				if (revised[i]) {
					for (Constraint c : constraint.getVariable(i)
							.getInvolvingConstraints()) {
						if (c != constraint) {
							addInQueue(c);
						}
					}
				}
			}
			Arrays.fill(revised, false);
		}

		return true;

	}

	private void clearQueue() {
		queue.clear();
		cstInQueue.clear();
	}

	private void addAll() {
		for (Constraint c : problem.getConstraints()) {
			addInQueue(c);
		}
	}

	private Constraint pullConstraint() {
		Constraint bestConstraint = queue.pollFirst();
		cstInQueue.clear(bestConstraint.getId());
		return bestConstraint;
	}

	private void addInQueue(final Constraint cst) {
		sizes[cst.getId()] = size(cst);
		if (!cstInQueue.get(cst.getId())) {
			cstInQueue.set(cst.getId());
			queue.add(cst);
		}
	}

	private void addInQueue(final Variable var) {
		for (Constraint c : var.getInvolvingConstraints()) {
			addInQueue(c);
		}
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
	public void setParameter(final int parameter) {
		// No parameter

	}

	@Override
	public boolean ensureAC() {
		return true;
	}

}
