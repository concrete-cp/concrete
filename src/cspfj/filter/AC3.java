package cspfj.filter;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.Constraint;
import cspfj.heuristic.VariableHeuristic;
import cspfj.heuristic.WeightHeuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {
	private final Problem problem;

	private final BitSet inQueue;

	private boolean removals[][];

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	private WeightHeuristic wvh;

	public AC3(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new BitSet(problem.getMaxVId() + 1);

		wvh = (heuristic instanceof WeightHeuristic) ? (WeightHeuristic) heuristic
				: null;

	}

	public boolean reduceAll(final int level) {
		addAll();
		return reduce(level);

	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
				.nextSetBit(i + 1)) {
			for (Constraint c : problem.getVariable(i)
					.getInvolvingConstraints()) {
				Arrays.fill(removals[c.getId()], false);
			}
		}

		inQueue.clear();

		inQueue.set(variable.getId());

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			this.removals[involving[cp].getId()][variable
					.getPositionInConstraint(cp)] = true;

		}

		return reduce(level);
	}

	private boolean reduce(final int level) {
		logger.finer("Reducing");
		final BitSet inQueue = this.inQueue;

		final boolean[] revised = new boolean[problem.getMaxArity()];

		while (!inQueue.isEmpty()) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {
				final Constraint constraint = constraints[c];

				final int position = variable.getPositionInConstraint(c);

				final boolean[] removals = this.removals[constraint.getId()];

				if (!removals[position]) {
					Arrays.fill(removals, false);
					continue;
				}

				if (!constraint.revise(level, revised)) {
					effectiveRevisions++;
					if (wvh != null) {
						wvh.treatConflictConstraint(constraint);
					}
					Arrays.fill(removals, false);
					return false;
				}

				boolean atLeastOne = false;

				for (int i = constraint.getArity(); --i >= 0;) {
					if (!revised[i]) {
						continue;
					}
					atLeastOne = true;
					final Variable y = constraint.getVariable(i);
					inQueue.set(y.getId());
					final Constraint[] involvingConstraints = y
							.getInvolvingConstraints();

					for (int cp = involvingConstraints.length; --cp >= 0;) {
						final Constraint constraintP = involvingConstraints[cp];
						if (constraintP != constraint) {
							this.removals[constraintP.getId()][y
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
				Arrays.fill(removals, false);

			}

		}

		return true;

	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			inQueue.set(v.getId());
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

	private Variable pullVariable() {
		Variable bestVariable = null;
		int bestValue = Integer.MAX_VALUE;

		final BitSet inQueue = this.inQueue;

		final Problem problem = this.problem;

		for (int i = inQueue.nextSetBit(0); i >= 0; i = inQueue
				.nextSetBit(i + 1)) {
			final Variable variable = problem.getVariable(i);
			final int domainSize = variable.getDomainSize();
			if (domainSize < bestValue) {
				bestVariable = variable;
				bestValue = domainSize;
			}
		}

		inQueue.clear(bestVariable.getId());
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
	public void setParameter(final int parameter) {
		// No parameter

	}

	@Override
	public boolean ensureAC() {
		return true;
	}
}
