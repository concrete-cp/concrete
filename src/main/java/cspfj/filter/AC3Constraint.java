package cspfj.filter;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Queue;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.priorityqueues.Fifo;
import cspfj.priorityqueues.Key;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3Constraint implements Filter {

	private final Problem problem;

	private final Queue<Constraint> queue;

	private static final Logger LOGGER = Logger.getLogger(Filter.class
			.getSimpleName());

	private int revisions = 0;

	private int revisionCount = 0;

	public AC3Constraint(final Problem problem) {
		super();
		this.problem = problem;

		queue = new Fifo<Constraint>(new Key<Constraint>() {
			@Override
			public double getKey(final Constraint o1) {
				return o1.getEvaluation(AC3Constraint.this.revisionCount)
						/ o1.getWeight();
			}
		}, problem.getNbConstraints());
	}

	public boolean reduceAll() {
		queue.clear();
		addAll();
		return reduce();

	}

	public boolean reduceFrom(final int[] modVar, final int[] modCons,
			final int cnt) {
		revisionCount++;
		queue.clear();
		LOGGER.fine("reduce after " + cnt);
		for (Variable v : problem.getVariables()) {
			if (modVar[v.getId()] > cnt) {
				final Constraint[] involved = v.getInvolvingConstraints();
				for (int j = involved.length; --j >= 0;) {
					final Constraint constraint = involved[j];
					if (!constraint.isEntailed()) {
						constraint.setRemovals(v.getPositionInConstraint(j),
								revisionCount);

						queue.offer(involved[j]);
					}
				}
			}
		}

		if (modCons != null) {
			for (Constraint c : problem.getConstraints()) {
				if (modCons[c.getId()] > cnt && !c.isEntailed()) {
					c.fillRemovals(revisionCount);

					queue.offer(c);
				}
			}
		}

		return reduce();
	}

	public boolean reduceAfter(final Variable variable) {
		revisionCount++;
		if (variable == null) {
			return true;
		}
		queue.clear();

		final Constraint[] involving = variable.getInvolvingConstraints();

		for (int cp = involving.length; --cp >= 0;) {
			if (!involving[cp].isEntailed()) {
				involving[cp].setRemovals(variable.getPositionInConstraint(cp),
						revisionCount);
				queue.offer(involving[cp]);
			}
		}

		return reduce();
	}

	private RevisionHandler revisator = new RevisionHandler() {
		public void revised(final Constraint constraint, final Variable variable) {
			final Constraint[] involvingConstraints = variable
					.getInvolvingConstraints();

			for (int cp = involvingConstraints.length; --cp >= 0;) {
				final Constraint constraintP = involvingConstraints[cp];
				if (constraintP != constraint && !constraintP.isEntailed()) {
					constraintP.setRemovals(variable
							.getPositionInConstraint(cp), revisionCount);
					queue.offer(constraintP);
				}

			}
		}
	};

	private boolean reduce() {
		LOGGER.finer("Reducing");
		final RevisionHandler revisator = this.revisator;

		while (!queue.isEmpty()) {
			final Constraint constraint = queue.poll();

			revisions++;
			if (!constraint.revise(revisator, revisionCount)) {
				constraint.incWeight();
				return false;
			}

			constraint.fillRemovals(-1);
		}

		assert control();

		return true;

	}

	private void addAll() {
		for (Constraint c : problem.getConstraints()) {
			if (!c.isEntailed()) {
				c.fillRemovals(revisionCount);
				queue.offer(c);
			}
		}
	}

	private boolean control() {

		final RevisionHandler controlRevisator = new RevisionHandler() {

			@Override
			public void revised(final Constraint constraint,
					final Variable variable) {
				assert false;

			}

		};

		for (Constraint c : problem.getConstraints()) {
			assert c.revise(controlRevisator, -1);
		}
		return true;
	}

	public String toString() {
		return "GAC3rm-cons+" + queue.getClass().getSimpleName();
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>(1);
		statistics.put("revisions", revisions);
		return statistics;
	}

	@Override
	public boolean reduceAfter(final Collection<Constraint> constraints) {
		revisionCount++;
		queue.clear();

		for (Constraint c : constraints) {
			c.fillRemovals(revisionCount);
			queue.offer(c);
		}

		return reduce();
	}

}
