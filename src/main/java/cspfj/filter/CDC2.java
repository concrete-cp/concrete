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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.AbstractSolver;
import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.constraint.extension.ExtensionConstraintDynamic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

/**
 * @author Julien VION
 * 
 */
public final class CDC2 implements Filter {

	private final static Logger logger = Logger.getLogger(CDC2.class.getName());

	private final static Problem.LearnMethod addConstraints = AbstractSolver.parameters
			.containsKey("cdc.addConstraints") ? Problem.LearnMethod
			.valueOf(AbstractSolver.parameters.get("cdc.addConstraints"))
			: Problem.LearnMethod.NONE;

	private int addedConstraints = 0;

	private int nbSingletonTests = 0;

	private final Problem problem;

	private AC3Arc filter;

	private final Variable[] variables;

	private final int[][] reviseMe;

	private final BitVector[] inQueue;

	private int nbNoGoods = 0;

	public CDC2(Problem problem) {
		this.problem = problem;
		this.variables = problem.getVariables();
		this.filter = new AC3Arc(problem);
		reviseMe = new int[problem.getMaxVId() + 1][];
		inQueue = new BitVector[problem.getMaxVId() + 1];
		for (Variable v : variables) {
			reviseMe[v.getId()] = new int[v.getInvolvingConstraints().length];
			inQueue[v.getId()] = BitVector.factory(
					v.getInvolvingConstraints().length, false);
		}

	}

	public boolean reduceAll() throws InterruptedException {
		final AC3Arc filter = this.filter;

		ExtensionConstraintDynamic.quick = true;
		if (!filter.reduceAll()) {
			ExtensionConstraintDynamic.quick = false;
			return false;
		}
		final Variable[] variables = this.variables;

		int mark = 0;

		int v = 0;
		final int nbC = problem.getNbConstraints();

		int cnt = 0;

		try {
			do {
				cnt++;
				final Variable variable = variables[v];
				// if (logger.isLoggable(Level.FINE)) {
				logger.info(variable.toString());
				// }
				if (variable.getDomainSize() > 1
						&& checkVar(variable, cnt)) {
					reviseMeToInQueueEqual(cnt);
					if (!filter.reduce(inQueue)) {
						return false;
					}
					mark = v;
				}
				if (++v >= variables.length) {
					v = 0;
				}
			} while (v != mark);
		} finally {
			addedConstraints += problem.getNbConstraints() - nbC;
			ExtensionConstraintDynamic.quick = false;
		}
		return true;
	}

	private void reviseMeToInQueueEqual(int cnt) {
		for (Variable v : variables) {
			for (int cp = v.getInvolvingConstraints().length; --cp >= 0;) {
				inQueue[v.getId()].set(cp, reviseMe[v.getId()][cp] == cnt);
			}
		}
	}

	private boolean checkVar(final Variable variable, int cnt)
			throws InterruptedException {
		boolean changedGraph = false;

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {
			if (Thread.interrupted()) {
				throw new InterruptedException();
			}

			// if (logger.isLoggable(Level.FINER)) {
			logger.fine(variable + " <- " + variable.getDomain().value(index)
					+ "(" + index + ")");
			// }

			problem.push();
			variable.assign(index, problem);

			nbSingletonTests++;

			if (filter.reduceAfter(variable)) {

				// final Map<Variable[], List<int[]>> noGoods =
				// problem.noGoods();
				changedGraph = noGoods(variable) | changedGraph;
				// logger.info(noGoods.toString());

				variable.unassign(problem);
				problem.pop();

				// changedGraph = problem.noGoodsToConstraints(noGoods,
				// addConstraints);
			} else {
				variable.unassign(problem);
				problem.pop();
				logger.fine("Removing " + variable + ", " + index);

				variable.remove(index);
				
				for (Constraint c: variable.getInvolvingConstraints()) {
					for (int p = c.getArity();--p>=0;) {
						if ()
					}
				}
				
				changedGraph = true;
			}
		}
		problem.setLevelVariables(null);
		return changedGraph;
	}

	public boolean noGoods(Variable firstVariable) {
		int[] tuple = new int[2];

		final Set<Variable> scopeSet = new HashSet<Variable>(2);

		scopeSet.add(firstVariable);
		tuple[0] = firstVariable.getFirst();
		final Variable[] scopeArray = new Variable[] { firstVariable, null };

		boolean modified = false;
		final Collection<Constraint> addedConstraints = new ArrayList<Constraint>();

		for (Variable fv : variables) {

			// logger.fine("checking " +
			// getVariable(levelVariables[level-1]));

			if (fv == firstVariable) {
				continue;
			}

			final BitVector changes = fv.getDomain().getAtLevel(0).exclusive(
					fv.getDomain().getAtLevel(1));
			if (changes.isEmpty()) {
				continue;
			}

			scopeSet.add(fv);
			final DynamicConstraint constraint = problem.learnConstraint(
					scopeSet, addConstraints);
			scopeSet.remove(fv);

			if (constraint == null) {
				continue;
			}

			scopeArray[1] = fv;

			final int[] base = new int[2];
			final int varPos = Problem.makeBase(scopeArray, tuple, constraint,
					base);

			int newNogoods = 0;
			for (int i = changes.nextSetBit(0); i >= 0; i = changes
					.nextSetBit(i + 1)) {
				base[varPos] = i;
				newNogoods += constraint.removeTuples(base);

			}
			if (newNogoods > 0) {
				nbNoGoods += newNogoods;
				modified = true;
				if (constraint.getId() > problem.getMaxCId()) {
					logger.info("Added " + constraint);
					addedConstraints.add(constraint);
				}
			}
		}

		if (modified) {
			logger.info(nbNoGoods + " nogoods");

			if (!addedConstraints.isEmpty()) {
				final Collection<Constraint> curCons = new ArrayList<Constraint>(
						problem.getConstraintsAsCollection());

				for (Constraint c : addedConstraints) {
					curCons.add(c);
				}

				problem.setConstraints(curCons);
				problem.updateInvolvingConstraints();
				logger.info(problem.getNbConstraints() + " constraints");
			}
		}
		return modified;
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("SAC-nbsingletontests", nbSingletonTests);
		for (Entry<String, Object> stat : filter.getStatistics().entrySet()) {
			statistics.put("SAC-backend-" + stat.getKey(), stat.getValue());
		}
		statistics.put("CDC-nogoods", nbNoGoods);
		statistics.put("CDC-added-constraints", addedConstraints);
		return statistics;
	}

	public String toString() {
		return "DC2 w/ " + filter + " L " + addConstraints;
	}

	@Override
	public boolean ensureAC() {
		return true;
	}

	@Override
	public boolean reduceAfter(Variable variable) {
		return false;
	}

	@Override
	public boolean reduceOnce(Variable variable) {
		return false;
	}
}
