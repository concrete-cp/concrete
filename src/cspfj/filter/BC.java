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
import java.util.HashMap;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class BC implements Filter {
	private final Problem problem;

	private final boolean[] inQueue;

	private int queueSize = 0;

	private int effectiveRevisions = 0;

	private int uselessRevisions = 0;

	public BC(final Problem problem) {
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

	private boolean reduce(final int level) {
		while (queueSize > 0) {
			final Variable variable = pullVariable();

			final Constraint[] constraints = variable.getInvolvingConstraints();

			for (int c = constraints.length; --c >= 0;) {

				final Constraint constraint = constraints[c];

				for (int i = constraint.getArity(); --i >= 0;) {
					final Variable y = constraint.getInvolvedVariables()[i];

					if (!y.isAssigned() && revise(constraint, i, level)) {
						if (y.getDomainSize() <= 0) {
							constraint.increaseWeight();
							return false;
						}

						addInQueue(y.getId());
					}
				}

			}

		}

		return true;

	}

	private void clearQueue() {
		Arrays.fill(inQueue, false);
		queueSize = 0;
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
		return "2Brm";
	}

	public boolean check(final Constraint constraint, final Variable variable,
			final int position, final int index, final int othersSize,
			final int level) {
		final long initConflicts = constraint.getNbInitConflicts(position,
				index);
		if (initConflicts >= 0 && othersSize > initConflicts) {
			return false;
		}
		if (!constraint.findValidTuple(position, index)) {
			// logger.finer("removing " + index + " from " + variable);

			variable.remove(index, level);
			constraint.setActive(true);
			return true;

		}
		return false;
	}

	public boolean revise(final Constraint constraint, final int position,
			final int level) {
		final Variable variable = constraint.getInvolvedVariables()[position];

		assert !variable.isAssigned();

		boolean revised = false;

		// logger.finer("Revising " + variable + " "
		// + Arrays.toString(variable.getCurrentDomain()) + " against "
		// + this);

		final int othersSize = constraint.getOtherSize(position);

		final long maxConflicts = constraint.getNbMaxConflicts(position);
		if (maxConflicts >= 0 && othersSize > maxConflicts) {
			return false;
		}

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {

			// logger.finer("Checking (" + variable + ", " + index + ")");
			if (!check(constraint, variable, position, index, othersSize, level)) {
				break;
			} else {
				revised = true;
			}

		}

		for (int index = variable.getLast(); index >= 0; index = variable
				.getPrev(index)) {
			if (!check(constraint, variable, position, index, othersSize, level)) {
				break;
			} else {
				revised = true;
			}

		}

		if (revised) {
			effectiveRevisions++;
		} else {
			uselessRevisions++;
		}

		return revised;
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("2b-effective-revisions", effectiveRevisions);
		statistics.put("2b-useless-revisions", uselessRevisions);
		return statistics;
	}

	@Override
	public void setParameter(int parameter) {
		// TODO Auto-generated method stub
		
	}
}
