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
import cspfj.util.BitVector;

/**
 * @author scand1sk
 * 
 */
public final class AC3Arc implements Filter {
	private final Problem problem;

	private final BitVector variableInQueue;

	private final BitVector[] perConstraintInQueue;

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	public AC3Arc(final Problem problem) {
		super();
		this.problem = problem;

		variableInQueue = BitVector.factory(problem.getMaxVId() + 1, false);
		perConstraintInQueue = new BitVector[problem.getMaxCId() + 1];

		for (Constraint c : problem.getConstraints()) {
			perConstraintInQueue[c.getId()] = BitVector.factory(c.getArity(),
					false);
		}

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
		}

	}

	public boolean reduceAll() {
		variableInQueue.fill(true);

		for (Constraint c : problem.getConstraints()) {
			perConstraintInQueue[c.getId()].fill(true);
		}
		return reduce();

	}

	public boolean reduceAfter(final Variable variable) {
		if (variable == null) {
			return true;
		}

		for (Constraint c : variable.getInvolvingConstraints()) {
			for (int p = c.getArity(); --p >= 0;) {
				if (c.getVariable(p) == variable) {
					continue;
				}
				variableInQueue.set(c.getVariable(p).getId());
				perConstraintInQueue[c.getId()].set(p);
			}
		}

		return reduce();
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {

			for (Constraint c : variable.getInvolvingConstraints()) {
				if (c == constraint) {
					continue;
				}
				for (int p = c.getArity(); --p >= 0;) {
					if (c.getVariable(p) == variable) {
						continue;
					}
					variableInQueue.set(c.getVariable(p).getId());
					perConstraintInQueue[c.getId()].set(p);
				}
			}

		}
	};

	public boolean reduce(BitVector variableInQueue,
			BitVector[] perConstraintInQueue) {
		variableInQueue.copyTo(this.variableInQueue);
		for (Constraint c : problem.getConstraints()) {
			perConstraintInQueue[c.getId()].copyTo(this.perConstraintInQueue[c
					.getId()]);
		}
		return reduce();
	}

	private boolean reduce() {
		logger.finer("Reducing");

		while (!variableInQueue.isEmpty()) {
			if (!reduceOnce(pullVariable())) {
				return false;
			}
		}

		assert control();

		return true;

	}

	public boolean reduceOnce(Variable variable) {
		final Constraint[] constraints = variable.getInvolvingConstraints();
		final RevisionHandler revisator = this.revisator;

		for (int c = constraints.length; --c >= 0;) {
			final Constraint constraint = constraints[c];
			if (!perConstraintInQueue[constraint.getId()].clear(variable
					.getPositionInConstraint(c))) {
				continue;
			}
			effectiveRevisions++;
			if (((AbstractPVRConstraint) constraints[c]).revise(variable
					.getPositionInConstraint(c))) {
				if (variable.getDomainSize() <= 0) {
					constraints[c].incWeight();
					clear();
					return false;
				}
				revisator.revised(constraint, variable);
			}
		}

		return true;
	}

	private void clear() {
		variableInQueue.fill(false);
		for (Constraint c : problem.getConstraints()) {
			perConstraintInQueue[c.getId()].fill(false);
		}
	}

	private boolean revised;

	private boolean control() {

		final RevisionHandler revisator = new RevisionHandler() {

			@Override
			public void revised(Constraint constraint, Variable variable) {
				revised = true;

			}

		};

		revised = false;

		for (Constraint c : problem.getConstraints()) {

			if (c instanceof AbstractPVRConstraint) {
				for (int i = c.getArity(); --i >= 0;) {
					assert c.getVariable(i).isAssigned()
							|| !((AbstractPVRConstraint) c).revise(i);
				}
			} else {
				c.revise(revisator);
				assert !revised;
			}
		}
		return true;
	}

	private Variable pullVariable() {
		Variable bestVariable = null;
		int bestValue = Integer.MAX_VALUE;

		final BitVector inQueue = this.variableInQueue;

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
		return "GAC3rm/Arc";
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
