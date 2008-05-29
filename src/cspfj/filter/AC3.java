/**
 * CSPFJ - CSP solving API for Java
 * Copyright (C) 2006 Julien VION
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package cspfj.filter;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

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

	private final boolean[] inQueue;

	private int queueSize = 0;

	private boolean removals[][];

	public int effectiveRevisions = 0;

	public int uselessRevisions = 0;

	private static final Logger logger = Logger.getLogger(Filter.class
			.getSimpleName());

	private WeightHeuristic wvh;

	public AC3(final Problem problem, final VariableHeuristic heuristic) {
		super();
		this.problem = problem;

		inQueue = new boolean[problem.getMaxVId() + 1];

		removals = new boolean[problem.getMaxCId() + 1][];
		for (Constraint c : problem.getConstraints()) {
			removals[c.getId()] = new boolean[c.getArity()];
		}

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

		addInQueue(variable.getId());

		return reduce(level);
	}

	private boolean skipRevision(final Constraint constraint,
			final int variablePosition) {
		if (!removals[constraint.getId()][variablePosition]
				|| constraint.getArity() == 1) {
			return false;
		}

		for (int y = constraint.getArity(); --y >= 0;) {
			if (y != variablePosition && removals[constraint.getId()][y]) {
				return false;
			}

		}

		return true;
	}

	private boolean reduce(final int level) {
		logger.fine("Reducing");
		while (queueSize > 0) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {

				final Constraint constraint = constraints[c];

				final int position = variable.getPositionInConstraint(c);
				if (!removals[constraint.getId()][position]) {
					continue;
				}

				final boolean[] revised = constraint.revise(level);

				if (revised == null) {

					for (int i = constraint.getArity(); --i >= 0;) {
						// if (i == position) {
						// continue;
						// }
						final Variable y = constraint.getVariable(i);

						if (!y.isAssigned() && !skipRevision(constraint, i)
								&& !constraint.skipRevision(i)) {
							if (constraint.revise(i, level)) {

								effectiveRevisions++;
								if (y.getDomainSize() <= 0) {
									if (wvh != null) {
										wvh.treatConflictConstraint(constraint);
									}
									return false;
								}

								addInQueue(y.getId());

								final Constraint[] involvingConstraints = y
										.getInvolvingConstraints();

								for (int cp = involvingConstraints.length; --cp >= 0;) {
									final Constraint constraintP = involvingConstraints[cp];
									if (constraintP != constraint) {
										removals[constraintP.getId()][y
												.getPositionInConstraint(cp)] = true;
									}
								}
							} else {
								uselessRevisions++;
							}
						}
					}
				} else {
					for (int i = constraint.getArity(); --i >= 0;) {
						if (revised[i]) {
							final Variable y = constraint.getVariable(i);

							if (y.getDomainSize() <= 0) {
								if (wvh != null) {
									wvh.treatConflictConstraint(constraint);
								}
								return false;
							}

							addInQueue(y.getId());
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

				}

				Arrays.fill(removals[constraint.getId()], false);
			}

		}

		return true;

	}

	private void clearQueue() {
		Arrays.fill(inQueue, false);
		queueSize = 0;
		for (Constraint c : problem.getConstraints()) {
			Arrays.fill(removals[c.getId()], true);
		}
	}

	private void addAll() {
		for (Variable v : problem.getVariables()) {
			addInQueue(v.getId());
		}
	}

	private Variable pullVariable() {
		Variable bestVariable = null;
		int bestValue = Integer.MAX_VALUE;

		final boolean[] inQueue = this.inQueue;

		final Problem problem = this.problem;

		for (int i = inQueue.length; --i >= 0;) {
			if (inQueue[i]) {
				final Variable variable = problem.getVariable(i);
				final int domainSize = variable.getDomainSize();
				if (domainSize < bestValue) {
					bestVariable = variable;
					bestValue = domainSize;
				}
			}
		}

		inQueue[bestVariable.getId()] = false;
		queueSize--;
		return bestVariable;
	}

	private void addInQueue(final int vId) {
		if (!inQueue[vId]) {
			inQueue[vId] = true;
			queueSize++;
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
