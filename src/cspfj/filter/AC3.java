package cspfj.filter;

import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.Constraint;
import cspfj.heuristic.VariableHeuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {
	private final Problem problem;

	private final BitSet inQueue;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new BitSet(problem.getMaxVId() + 1);
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
			final Variable v = problem.getVariable(i);
			final Constraint[] involved = v.getInvolvingConstraints();
			for (int j = involved.length; --j >= 0;) {
				involved[j].setRemovals(v.getPositionInConstraint(j), false);
			}
		}

		inQueue.clear();

		inQueue.set(variable.getId());

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			involving[cp].setRemovals(variable.getPositionInConstraint(cp),
					true);

		}

		return reduce(level);
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {
			inQueue.set(variable.getId());
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
		final BitSet inQueue = this.inQueue;
		final RevisionHandler revisator = this.revisator;

		while (!inQueue.isEmpty()) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {
				final Constraint constraint = constraints[c];

				final int position = variable.getPositionInConstraint(c);

//				if (!constraint.getRemovals(position)) {
//					constraint.fillRemovals(false);
//					continue;
//				}

				if (!constraint.revise(level, revisator)) {
					effectiveRevisions++;
					constraint.incWeight();
					constraint.fillRemovals(false);
					return false;
				}

				constraint.fillRemovals(false);

			}

		}

		assert control(level);

		return true;

	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			inQueue.set(v.getId());
		}

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}
	}

	private boolean control(int level) {
		for (Constraint c : problem.getConstraints()) {
			if (!(c instanceof AbstractPVRConstraint)) {
				continue;
			}
			for (int i = c.getArity(); --i >= 0;) {
				if (!c.getVariable(i).isAssigned()
						&& ((AbstractPVRConstraint) c).revise(i, level)) {
					return false;
				}
			}
		}
		return true;
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
		final Map<String, Object> statistics = new HashMap<String, Object>(2);
		statistics.put("gac3-effective-revisions", effectiveRevisions);
		statistics.put("gac3-useless-revisions", uselessRevisions);
		return statistics;
	}

	@Override
	public boolean ensureAC() {
		return true;
	}
}
