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

import cspfj.constraint.Constraint;
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

	public AC3(final Problem problem) {
		super();
		this.problem = problem;

		inQueue = new boolean[problem.getMaxVId() + 1];
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
		if (!constraint.getRemovals(variablePosition)) {
			return false;
		}

		for (int y = constraint.getArity(); --y >= 0;) {
			if (y != variablePosition && constraint.getRemovals(y)) {
				return false;
			}

		}

		return true;
	}

	private boolean reduce(final int level) {
		while (queueSize > 0) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {

				final Constraint constraint = constraints[c];

				if (!constraint
						.getRemovals(variable.getPositionInConstraint(c))) {
					continue;
				}

				for (int i = constraint.getArity(); --i >= 0;) {
					final Variable y = constraint.getInvolvedVariables()[i];

					if (!y.isAssigned() && !skipRevision(constraint, i)
							&& constraint.revise(i, level)) {
						if (y.getDomainSize() <= 0) {
							constraint.increaseWeight();
							return false;
						}

						addInQueue(y.getId());

						for (int cp = y.getInvolvingConstraints().length; --cp >= 0;) {
							final Constraint constraintP = y
									.getInvolvingConstraints()[cp];
							if (constraintP != constraint) {
								constraintP.setRemovals(y
										.getPositionInConstraint(cp), true);
							}
						}
					}
				}

				constraint.fillRemovals(false);
			}

		}

		return true;

	}

	private void clearQueue() {
		Arrays.fill(inQueue, false);
		queueSize = 0;
		for (Constraint c : problem.getConstraints()) {
			c.fillRemovals(true);
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

	public int getNbNoGoods() {
		return 0;
	}

	public String toString() {
		return "AC3rm";
	}

}
