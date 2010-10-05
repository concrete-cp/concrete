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
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.ParameterManager;
import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.NoGoodLearner.LearnMethod;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.BitVector;
import cspfj.util.Parameter;

/**
 * @author Julien VION
 * 
 */
public final class DC2 implements Filter {

	private static final Logger LOGGER = Logger.getLogger(DC2.class.getName());

	@Parameter("dc2.addConstraints")
	private static LearnMethod addConstraints = LearnMethod.CONS;

	static {
		ParameterManager.register(DC2.class);
	}
	private int nbAddedConstraints = 0;

	private int nbNoGoods;

	// private final List<DynamicConstraint> impliedConstraints;

	private final int[] modVar;
	private int[] modCons;

	private int cnt;
	private final AC3 filter;

	private final Problem problem;

	private int nbSingletonTests = 0;

	private final NoGoodLearner ngl;

	public DC2(final Problem problem) {
		this.problem = problem;
		this.filter = new AC3(problem);
		// impliedConstraints = new ArrayList<DynamicConstraint>();
		modCons = new int[problem.getMaxCId() + 1];
		modVar = new int[problem.getMaxVId() + 1];
		ngl = new NoGoodLearner(problem, addConstraints);
	}

	@Override
	public boolean reduceAll() throws InterruptedException {
		final int nbC = problem.getNbConstraints();

		// for (Constraint c : problem.getConstraints()) {
		// if (c.getArity() == 2
		// && DynamicConstraint.class.isAssignableFrom(c.getClass())) {
		// impliedConstraints.add((DynamicConstraint) c);
		// }
		// }

		// ExtensionConstraintDynamic.quick = true;
		final boolean result;
		try {
			result = cdcReduce();
		} finally {
			nbAddedConstraints += problem.getNbConstraints() - nbC;
		}
		// ExtensionConstraintDynamic.quick = false;
		// for (Constraint c : problem.getConstraints()) {
		// if (c instanceof RCConstraint) {
		// ((RCConstraint) c).flushPending();
		// }
		// }
		return result;
	}

	private boolean cdcReduce() throws InterruptedException {
		if (!filter.reduceAll()) {
			return false;
		}
		final Variable[] variables = problem.getVariables();

		int mark = 0;

		int v = 0;

		final int[] domainSizes = new int[problem.getMaxVId() + 1];

		do {
			final Variable variable = variables[v];
			// if (logger.isLoggable(Level.FINE)) {
			LOGGER.info(variable.toString());
			// }
			cnt++;
			if (variable.getDomainSize() > 1 && singletonTest(variable)) {
				if (variable.getDomainSize() <= 0) {
					return false;
				}

				for (Variable var : problem.getVariables()) {
					domainSizes[var.getId()] = var.getDomainSize();
				}
				if (!filter.reduceFrom(modVar, modCons, cnt - 1)) {
					return false;
				}
				// if (!filter.reduceAll()) {
				// return false;
				// }
				for (Variable var : problem.getVariables()) {
					if (domainSizes[var.getId()] != var.getDomainSize()) {
						modVar[var.getId()] = cnt;
					}
				}
				mark = v;
			}
			if (++v >= variables.length) {
				v = 0;
			}
		} while (v != mark);

		return true;

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
			LOGGER.fine(variable + " <- " + variable.getDomain().value(index));
			// }

			problem.push();
			variable.setSingle(index);

			nbSingletonTests++;

			final boolean sat;

			if (cnt <= problem.getNbVariables()) {
				sat = filter.reduceAfter(variable);
			} else {
				final Constraint[] involving = variable
						.getInvolvingConstraints();

				/*
				 * Forward checking !
				 */
				for (int i = involving.length; --i >= 0;) {
					final Constraint c = involving[i];
					if (c.getArity() != 2) {
						continue;
					}

					c.revise(rh, -1);

					c.fillRemovals(-1);
				}

				sat = filter.reduceFrom(modVar, modCons,
						cnt - problem.getNbVariables());
			}
			if (sat) {

				// final Map<Variable[], List<int[]>> noGoods =
				// problem.noGoods();
				changedGraph = noGoods(variable) | changedGraph;
				// logger.info(noGoods.toString());

				problem.pop();

				// changedGraph = problem.noGoodsToConstraints(noGoods,
				// addConstraints);
			} else {
				problem.pop();
				LOGGER.fine("Removing " + variable + ", " + index);

				variable.remove(index);
				changedGraph = true;
				modVar[variable.getId()] = cnt;
			}
		}
		return changedGraph;
	}

	private final RevisionHandler rh = new RevisionHandler() {
		@Override
		public void revised(final Constraint constraint, final Variable variable) {
			LOGGER.fine("FC w " + constraint + ", " + variable);
		}
	};

	public boolean noGoods(final Variable firstVariable) {
		final List<Integer> tuple = new ArrayList<Integer>(2);

		final Set<Variable> scopeSet = new HashSet<Variable>(2);

		scopeSet.add(firstVariable);
		tuple.add(firstVariable.getFirst());
		final Variable[] scopeArray = new Variable[] { firstVariable, null };

		boolean modified = false;
		final Collection<DynamicConstraint> addedConstraints = new ArrayList<DynamicConstraint>();

		for (Variable fv : problem.getVariables()) {

			// logger.fine("checking " +
			// getVariable(levelVariables[level-1]));

			if (fv == firstVariable) {
				continue;
			}

			final BitVector changes = fv.getDomain().getAtLevel(0)
					.xor(fv.getDomain().getAtLevel(1));
			if (changes.isEmpty()) {
				continue;
			}

			scopeSet.add(fv);
			final DynamicConstraint constraint = ngl.learnConstraint(scopeSet);
			scopeSet.remove(fv);

			if (constraint == null) {
				continue;
			}

			scopeArray[1] = fv;

			final int[] base = new int[constraint.getArity()];
			final int varPos = NoGoodLearner.makeBase(scopeArray, tuple,
					constraint, base);

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
					LOGGER.info("Added " + constraint);
					addedConstraints.add(constraint);
					modCons = Arrays.copyOf(modCons, constraint.getId() + 1);
				}

				modCons[constraint.getId()] = cnt;
			}
		}

		if (modified) {
			LOGGER.fine(nbNoGoods + " nogoods");

			if (!addedConstraints.isEmpty()) {
				for (Constraint c : addedConstraints) {
					problem.addConstraint(c);
				}

				problem.prepare();

				LOGGER.info(problem.getNbConstraints() + " constraints");
			}
		}
		return modified;
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("CDC-nbsingletontests", nbSingletonTests);
		for (Entry<String, Object> stat : filter.getStatistics().entrySet()) {
			statistics.put("CDC-backend-" + stat.getKey(), stat.getValue());
		}
		statistics.put("CDC-nogoods", nbNoGoods);
		statistics.put("CDC-added-constraints", nbAddedConstraints);
		return statistics;
	}

	public String toString() {
		return "DC w/ " + filter + " L " + ngl.getLearnMethod();
	}

	@Override
	public boolean reduceAfter(final Variable variable) {
		if (variable == null) {
			return true;
		}
		try {
			return reduceAll();
		} catch (InterruptedException e) {
			throw new IllegalStateException(
					"Filter was unexpectingly interrupted !", e);
		}
	}

	@Override
	public boolean reduceAfter(Collection<Constraint> constraints) {
		throw new UnsupportedOperationException();
	}

}
