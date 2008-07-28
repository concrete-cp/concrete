package cspfj.filter;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
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
public final class AC3Heap implements Filter {
	private final Problem problem;

	private final boolean[] inQueue;
	private Heap<Variable> queue;

	private boolean removals[][];

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private final boolean[] revised;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	private WeightHeuristic wvh;

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
		wvh = (heuristic instanceof WeightHeuristic) ? (WeightHeuristic) heuristic
				: null;

		revised = new boolean[problem.getMaxArity()];

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
					Arrays.fill(removals[c.getId()], false);
				}
				inQueue[i] = false;
			}
		}

		queue.clear();

		inQueue[variable.getId()] = true;
		queue.add(variable);
		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			this.removals[involving[cp].getId()][variable
					.getPositionInConstraint(cp)] = true;

		}

		return reduce(level);
	}

	private boolean reduce(final int level) {
		logger.fine("Reducing");

		final Heap<Variable> queue = this.queue;
		final boolean[] inQueue = this.inQueue;

		final boolean[] revised = this.revised;
		final boolean[][] removals = this.removals;

		while (!queue.isEmpty()) {
			final Variable variable = queue.pollFirst();
			inQueue[variable.getId()] = false;

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {
				final Constraint constraint = constraints[c];

				final int position = variable.getPositionInConstraint(c);

				final boolean[] constraintRemovals = removals[constraint
						.getId()];

				if (!constraintRemovals[position]) {
					Arrays.fill(constraintRemovals, false);
					continue;
				}

				if (!constraint.revise(level, revised)) {
					effectiveRevisions++;
					if (wvh != null) {
						wvh.treatConflictConstraint(constraint);
					}
					Arrays.fill(constraintRemovals, false);
					return false;
				}

				boolean atLeastOne = false;

				for (int i = constraint.getArity(); --i >= 0;) {
					if (!revised[i]) {
						continue;
					}
					atLeastOne = true;
					final Variable y = constraint.getVariable(i);
					if (!inQueue[y.getId()]) {
						inQueue[y.getId()] = true;
						queue.add(y);
					}
					final Constraint[] involvingConstraints = y
							.getInvolvingConstraints();

					for (int cp = involvingConstraints.length; --cp >= 0;) {
						final Constraint constraintP = involvingConstraints[cp];
						if (constraintP != constraint) {
							removals[constraintP.getId()][y
									.getPositionInConstraint(cp)] = true;
						}

					}

				}
				if (atLeastOne) {
					effectiveRevisions++;
				} else {
					uselessRevisions++;
				}
				Arrays.fill(revised, false);
				Arrays.fill(constraintRemovals, false);

			}

		}

		return true;

	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			inQueue[v.getId()] = true;
			queue.add(v);
		}
		if (removals == null || removals.length < problem.getMaxCId() + 1) {
			removals = new boolean[problem.getMaxCId() + 1][];
			for (Constraint c : problem.getConstraints()) {
				final boolean[] subRemovals = new boolean[c.getArity()];
				removals[c.getId()] = subRemovals;
				if (c instanceof AbstractPVRConstraint) {
					((AbstractPVRConstraint) c).setRemovals(subRemovals);
				}
			}
		}
		for (Constraint c : problem.getConstraints()) {
			Arrays.fill(removals[c.getId()], true);
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
	public void setParameter(final int parameter) {
		// No parameter

	}

	@Override
	public boolean ensureAC() {
		return true;
	}
}
