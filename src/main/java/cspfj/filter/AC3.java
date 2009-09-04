package cspfj.filter;

import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.logging.Logger;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.Constraint;
import cspfj.priorityqueues.BinomialHeap;
import cspfj.priorityqueues.Key;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {
	private final Problem problem;

	private final Queue<Variable> queue;

	private static final Logger LOGGER = Logger.getLogger(Filter.class
			.getSimpleName());

	private int revisions = 0;

	private int reviseCount = 0;

	public AC3(final Problem problem) {
		super();
		this.problem = problem;

		queue = new BinomialHeap<Variable>(new Key<Variable>() {
			@Override
			public int getKey(Variable o1) {
				return o1.getDomainSize();
			}
		}, problem.getNbVariables());
	}

	public boolean reduceAll() {
		queue.clear();
		addAll();
		return reduce();

	}

	public boolean reduceFrom(int[] modVar, int[] modCons, int cnt) {
		reviseCount++;
		queue.clear();
		LOGGER.fine("reduce after " + cnt);
		for (Variable v : problem.getVariables()) {
			if (modVar[v.getId()] > cnt) {
				queue.offer(v);
			}
		}

		if (modCons != null) {
			// final BitSet cons = new BitSet();
			for (Constraint c : problem.getConstraints()) {
				if (modCons[c.getId()] > cnt) {
					// cons.set(c.getId());
					c.fillRemovals(reviseCount);

					if (!c.revise(revisator, reviseCount)) {
						c.incWeight();
						return false;
					}
					c.fillRemovals(-1);
				}
			}

			// for (int i = modCons.length; --i >= 0;) {
			// assert modCons[i] <= cnt || cons.get(i);
			// }
		}

		return reduce();
	}

	public boolean reduceAfter(final Variable variable) {
		reviseCount++;
		if (variable == null) {
			return true;
		}
		queue.clear();

		queue.offer(variable);

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			involving[cp].setRemovals(variable.getPositionInConstraint(cp),
					reviseCount);

		}

		return reduce();
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {
			queue.offer(variable);
			final Constraint[] involvingConstraints = variable
					.getInvolvingConstraints();

			for (int cp = involvingConstraints.length; --cp >= 0;) {
				final Constraint constraintP = involvingConstraints[cp];
				if (constraintP != constraint) {
					constraintP.setRemovals(variable
							.getPositionInConstraint(cp), reviseCount);
				}

			}
		}
	};

	private boolean reduce() {
		LOGGER.finer("Reducing");

		while (!queue.isEmpty()) {
			if (!reduceOnce(queue.poll())) {
				return false;
			}
		}

		assert control();

		return true;

	}

	public boolean reduceOnce(Variable variable) {

		final RevisionHandler revisator = this.revisator;
		final Constraint[] involvingConstraints = variable
				.getInvolvingConstraints();
		for (int c = involvingConstraints.length; --c >= 0;) {
			final Constraint constraint = involvingConstraints[c];
			if (constraint.hasNoRemovals(reviseCount)) {
				continue;
			}
			revisions++;
			if (!constraint.revise(revisator, reviseCount)) {
				constraint.incWeight();
				return false;
			}

			constraint.fillRemovals(-1);

		}
		return true;
	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			queue.offer(v);
		}

		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(reviseCount);
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
					if (!c.getVariable(i).isAssigned()) {
						assert !((AbstractPVRConstraint) c).revise(i) : c
								+ ", " + c.getVariable(i);
					}
				}
			} else {
				assert c.revise(revisator, -1);
				assert !revised;
			}

		}
		return true;
	}

	public String toString() {
		return "GAC3rm-variable";
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>(1);
		statistics.put("revisions", revisions);
		return statistics;
	}

}
