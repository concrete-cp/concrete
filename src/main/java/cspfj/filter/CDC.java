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
import java.util.logging.Logger;

import cspfj.AbstractSolver;
import cspfj.constraint.extension.ExtensionConstraintDynamic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class CDC extends AbstractSAC {

	private final static Logger logger = Logger.getLogger(CDC.class.getName());

	private final static Problem.LearnMethod addConstraints = AbstractSolver.parameters
			.containsKey("cdc.addConstraints") ? Problem.LearnMethod
			.valueOf(AbstractSolver.parameters.get("cdc.addConstraints"))
			: Problem.LearnMethod.NONE;

	private int addedConstraints = 0;

	public CDC(Problem problem, Filter filter) {
		super(problem, filter);
	}

	@Override
	protected boolean reduce() throws InterruptedException {
		final int nbC = problem.getNbConstraints();
		ExtensionConstraintDynamic.quick = true;
		final boolean result;
		try {
			result = super.reduce();
		} finally {
			addedConstraints += problem.getNbConstraints() - nbC;
		}
		ExtensionConstraintDynamic.quick = false;
		// for (Constraint c : problem.getConstraints()) {
		// if (c instanceof RCConstraint) {
		// ((RCConstraint) c).flushPending();
		// }
		// }
		return result;
	}

	protected boolean singletonTest(final Variable variable)
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
			logger.fine(variable + " <- " + variable.getDomain().value(index)
					+ "(" + index + ")");
			// }
			
			problem.setLevelVariables(variable);
			problem.push();
			variable.assign(index, problem);
			
			nbSingletonTests++;

			if (filter.reduceAfter(variable)) {

//				final Map<Variable[], List<int[]>> noGoods = problem.noGoods();
				changedGraph = problem.noGoods(addConstraints) | changedGraph;
				// logger.info(noGoods.toString());

				variable.unassign(problem);
				problem.pop();
				
//				changedGraph = problem.noGoodsToConstraints(noGoods, addConstraints);
			} else {
				variable.unassign(problem);
				problem.pop();
				logger.fine("Removing " + variable + ", " + index);

				variable.remove(index);
				changedGraph = true;
			}
		}
		problem.setLevelVariables(null);
		return changedGraph;
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = super.getStatistics();
		statistics.put("CDC-nogoods", problem.getNbNoGoods());
		statistics.put("CDC-added-constraints", addedConstraints);
		return statistics;
	}

	public String toString() {
		return "DC w/ " + filter + " L " + addConstraints;
	}
}
