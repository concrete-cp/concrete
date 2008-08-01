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
public final class AC3WSlowQueue implements Filter {
	private final Problem problem;

	private final BitSet inQueue;

	private final Heap<Constraint> slowQueue;

	// private int queueSize = 0;

	private final BitSet cstInQueue;

	private final Heap<Variable> varQueue;

	private boolean removals[][];

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	private WeightHeuristic wvh;

	public AC3WSlowQueue(final Problem problem,
			final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new BitSet(problem.getMaxVId() + 1);
		cstInQueue = new BitSet();
		removals = new boolean[problem.getMaxCId() + 1][];
		int nbSlowCst = 0;
		for (Constraint c : problem.getConstraints()) {
			removals[c.getId()] = new boolean[c.getArity()];
			if (c.isSlow()) {
				nbSlowCst++;
			}
		}

		slowQueue = new Heap<Constraint>(new Comparator<Constraint>() {
			@Override
			public int compare(Constraint arg0, Constraint arg1) {
				return -Double.compare(size(arg0), size(arg1));
			}

			private double size(Constraint arg0) {
				double size = 0;
				for (int i = arg0.getArity(); --i >= 0;) {
					size += Math.log(arg0.getVariable(i).getDomainSize());
				}
				return size;
			}
		}, new Constraint[nbSlowCst]);
		varQueue = new Heap<Variable>(new Comparator<Variable>() {
			@Override
			public int compare(Variable arg0, Variable arg1) {
				return arg0.getDomainSize() - arg1.getDomainSize();
			}
		}, new Variable[problem.getNbVariables()]);

		wvh = (heuristic instanceof WeightHeuristic) ? (WeightHeuristic) heuristic
				: null;

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

		addInQueue(variable);

		return reduce(level);
	}

	private boolean reduce(final int level) {
		logger.fine("Reducing");
		final boolean[] revised = new boolean[problem.getMaxArity()];

		while (!varQueue.isEmpty() || !slowQueue.isEmpty()) {
			if (varQueue.isEmpty()) {
				final Constraint constraint = pullConstraint();

				if (!revise(constraint, level, revised, true)) {
					if (wvh != null) {
						wvh.treatConflictConstraint(constraint);
					}
					return false;
				}

				for (int i = constraint.getArity(); --i >= 0;) {
					if (revised[i]) {
						final Variable y = constraint.getVariable(i);
						addInQueue(y);
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
				}

				Arrays.fill(removals[constraint.getId()], false);

			} else {
				final Variable variable = pullVariable();

				final Constraint[] constraints = variable
						.getInvolvingConstraints();

				for (int c = constraints.length; --c >= 0;) {
					final Constraint constraint = constraints[c];

					final int position = variable.getPositionInConstraint(c);
					if (!removals[constraint.getId()][position]) {
						continue;
					}

					if (!revise(constraint, level, revised, false)) {
						if (wvh != null) {
							wvh.treatConflictConstraint(constraint);
						}
						return false;
					}

					for (int i = constraint.getArity(); --i >= 0;) {
						if (revised[i]) {
							final Variable y = constraint.getVariable(i);
							addInQueue(y);
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
					}

					Arrays.fill(removals[constraint.getId()], false);

				}
			}

		}

		return true;

	}

	private boolean revise(Constraint constraint, int level,
			final boolean[] revised, final boolean slow) {
		if (!slow && constraint.isSlow()) {
			addInQueue(constraint);
			return true;
		}
		return constraint.revise(level, revised);
	}

	private void clearQueue() {
		varQueue.clear();
		inQueue.clear();
		// queueSize = 0;
		for (Constraint c : problem.getConstraints()) {
			Arrays.fill(removals[c.getId()], true);
		}
		slowQueue.clear();
		cstInQueue.clear();
	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			addInQueue(v);
		}
	}

	private Variable pullVariable() {
		// Variable bestVariable = null;
		// int bestValue = Integer.MAX_VALUE;
		//
		// final boolean[] inQueue = this.inQueue;
		//
		// final Problem problem = this.problem;
		//
		// for (int i = inQueue.length; --i >= 0;) {
		// if (inQueue[i]) {
		// final Variable variable = problem.getVariable(i);
		// final int domainSize = variable.getDomainSize();
		// if (domainSize < bestValue) {
		// bestVariable = variable;
		// bestValue = domainSize;
		// }
		// }
		// }
		//
		Variable bestVariable = varQueue.pollFirst();
		inQueue.clear(bestVariable.getId());
		// queueSize--;
		return bestVariable;
	}

	private Constraint pullConstraint() {
		Constraint bestConstraint = slowQueue.pollFirst();
		cstInQueue.clear(bestConstraint.getId());
		return bestConstraint;
	}

	private void addInQueue(final Constraint cst) {
		if (!cstInQueue.get(cst.getId())) {
			cstInQueue.set(cst.getId());
			slowQueue.add(cst);
		}
	}

	private void addInQueue(final Variable var) {
		if (!inQueue.get(var.getId())) {
			inQueue.set(var.getId());
			// inQueue.set(var.getId());
			varQueue.add(var);
			// queueSize++;
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
