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

import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.AbstractSolver;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class CDC extends AbstractSAC {

	private final static Logger logger = Logger.getLogger(CDC.class.getName());

	private final boolean addConstraints;

	private int addedConstraints = 0;

	public CDC(Problem problem, Filter filter) {
		super(problem, filter);
		addConstraints = AbstractSolver.parameters
				.containsKey("cdc.addConstraints")
				&& Boolean.valueOf(AbstractSolver.parameters
						.get("cdc.addConstraints"));
	}

	@Override
	protected boolean reduce(int level) throws InterruptedException {
		final int nbC = problem.getNbConstraints();
		final boolean result;
		try {
			result = super.reduce(level);
		} finally {
			addedConstraints += problem.getNbConstraints() - nbC;
		}
		return result;
	}

	protected boolean singletonTest(final Variable variable, final int level)
			throws InterruptedException {
		boolean changedGraph = false;
		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {
			if (Thread.interrupted()) {
				throw new InterruptedException();
			}
			if (!variable.isPresent(index)) {
				continue;
			}

			// if (logger.isLoggable(Level.FINER)) {
			logger.finer(level + " : " + variable + " <- "
					+ variable.getDomain()[index] + "(" + index + ")");
			// }

			variable.assign(index, problem);
			problem.setLevelVariables(level, variable);
			nbSingletonTests++;
			if (filter.reduceAfter(level + 1, variable)) {

				if (problem.addNoGoods(addConstraints)) {
					changedGraph = true;
				}

				variable.unassign(problem);
				problem.restore(level + 1);
			} else {
				variable.unassign(problem);
				problem.restore(level + 1);
				logger.fine("Removing " + variable + ", " + index);

				variable.remove(index, level);
				changedGraph = true;
			}
		}
		return changedGraph;
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = super.getStatistics();
		statistics.put("CDC-nogoods", problem.getNbNoGoods());
		statistics.put("CDC-added-constraints", addedConstraints);
		return statistics;
	}

	public String toString() {
		return (addConstraints ? "C" : "") + "DC w/ " + filter;
	}
}
