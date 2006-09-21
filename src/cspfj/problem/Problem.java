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

package cspfj.problem;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;

public final class Problem {
	private Map<Integer, Variable> variables;

	private Variable[] variableArray;

	private Map<Integer, Constraint> constraints;

	private Constraint[] constraintArray;

	private int nbFutureVariables;

	private final static Logger logger = Logger.getLogger("cspfj.Problem");

	private int maxDomainSize;

	private int[] levelVariables;

	final private static ArrayList<Variable> scope = new ArrayList<Variable>();

	final private static ArrayList<Integer> tuple = new ArrayList<Integer>();

	final private static Set<Integer> pastVariables = new TreeSet<Integer>();

	private int maxArity;
	
	final private String name ;

	public Problem(String name) {
		super();
		this.name = name ;
	}

	public Problem(Collection<Variable> variables,
			Collection<Constraint> constraints, String name) {
		this(name);
		setVariables(variables);
		setConstraints(constraints);
		updateInvolvingConstraints();
	}

	public static Problem load(final ProblemGenerator generator)
			throws FailedGenerationException {
		final Problem problem = new Problem(generator.getName());
		Variable.resetVId();
		Constraint.resetCId();

		logger.fine("Generating");
		generator.generate();

		logger.fine("Setting Variables");
		problem.setVariables(generator.getVariables());
		logger.fine("Setting Constraints");
		problem.setConstraints(generator.getConstraints());

		logger.fine("Updating InvolvingConstraints");
		problem.updateInvolvingConstraints();

		// for (Constraint c :
		// generator.getVariables()[21].getInvolvingConstraints()) {
		// if (c.getInvolvedVariables()[1] == generator.getVariables()[23])
		// System.out.println(c);
		// }

		logger.fine("Done");
		return problem;

	}

	public int getNbFutureVariables() {
		return nbFutureVariables;
	}

	private void setVariables(final Collection<Variable> vars) {

		this.variables = new HashMap<Integer, Variable>(vars.size());

		this.variableArray = vars.toArray(new Variable[vars.size()]);

		maxDomainSize = 0;

		for (Variable var : vars) {
			variables.put(var.getId(), var);
			if (var.getDomain().length > maxDomainSize) {
				maxDomainSize = var.getDomain().length;
			}
		}

		nbFutureVariables = vars.size();

		levelVariables = new int[getNbVariables()];
		for (int i = 0; i < getNbVariables(); i++) {
			levelVariables[i] = -1;
		}

	}

	private void setConstraints(final Collection<Constraint> cons) {
		this.constraints = new HashMap<Integer, Constraint>(cons.size());

		this.constraintArray = cons.toArray(new Constraint[cons.size()]);

		for (Constraint c : cons) {
			this.constraints.put(c.getId(), c);
			if (c.getArity() > maxArity) {
				maxArity = c.getArity();
			}
		}

		// resetConstraint = new boolean[constraints.length];
	}

	public void updateInvolvingConstraints() {
		final Map<Integer, List<Constraint>> invConstraints = new HashMap<Integer, List<Constraint>>(
				variables.size());

		for (Variable v : getVariables()) {
			invConstraints.put(v.getId(), new ArrayList<Constraint>());
		}

		for (Constraint c : getConstraints()) {
			for (Variable v : c.getInvolvedVariables()) {
				invConstraints.get(v.getId()).add(c);
			}
		}

		for (Variable v : getVariables()) {

			v.setInvolvingConstraints(invConstraints.get(v.getId()).toArray(
					new Constraint[invConstraints.get(v.getId()).size()]));
		}

		// setValueOrders() ;
	}

	public int getNbVariables() {
		return variables.size();
	}

	public int getNbConstraints() {
		return constraints.size();
	}

	public Constraint[] getConstraints() {
		return constraintArray;
	}

	// public Collection<Integer> getConstraintIDs() {
	// return constraints.keySet();
	// }

	public Constraint getConstraint(final int cId) {
		return constraints.get(cId);
	}

	public Variable[] getVariables() {
		return variableArray;
	}

	// public Collection<Integer> getVariableIDs() {
	// return variables.keySet();
	// }

	public Variable getVariable(final int vId) {
		return variables.get(vId);
	}

	public void increaseFutureVariables() {
		nbFutureVariables++;
	}

	public void decreaseFutureVariables() {
		nbFutureVariables--;
	}

	public void restore(final int level) {
		// for (int i = 0; i < resetConstraint.length; i++) {
		// resetConstraint[i] = false;
		// }
		for (Variable v : getVariables()) {
			if (!v.isAssigned()) {
				v.restoreLevel(level);
			}

			// if (v.restore(level)) {
			// for (AbstractConstraint c : v.getInvolvingConstraints()) {
			// if (!resetConstraint[c.getId()]) {
			// c.initLast();
			// resetConstraint[c.getId()] = true;
			// }
			// }
			// }
		}

	}

	public void restoreAll(final int level) {
		for (Variable v : getVariables()) {
			if (v.isAssigned()) {
				v.unassign(this);
			}

			v.restoreLevel(level);

		}

		assert getNbFutureVariables() == getNbVariables();
		// for (AbstractConstraint c : constraints) {
		// c.initLast();
		// }
	}

	public void setValueOrders(final Random random) {
		// logger.info("Initializing value orders");
		for (Variable v : getVariables()) {
			v.orderIndexes(random);
		}

	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		for (Variable v : getVariables()) {
			sb.append(v.toString());
			sb.append(" : ");
			sb.append(Arrays.toString(v.getCurrentDomain()));
			sb.append('\n');
		}
		for (Constraint c : getConstraints()) {
			sb.append(c.toString());
			sb.append('\n');
		}

		return sb.toString();
	}

	public int getMaxDomainSize() {
		return maxDomainSize;
	}

	public boolean addNoGood() {

		final List<Variable> scope = Problem.scope;

		final Constraint constraint = Constraint.findConstraint(scope, Arrays
				.asList(scope.get(0).getInvolvingConstraints()));

		if (constraint == null) {
			return false;
			// constraint = new ExtensionConstraint(scope
			// .toArray(new Variable[scope.size()]));
			// final Constraint[] newConstraints = new
			// Constraint[constraints.length + 1];
			// System.arraycopy(constraints, 0, newConstraints, 1,
			// constraints.length);
			// newConstraints[0] = constraint;
			// constraints = newConstraints;
			// updateInvolvingConstraints();
			// logger.fine("Creating constraint " + constraint + " ("
			// + constraints.length + " constraints)");
		}

		return constraint.removeTuple(scope, tuple);
	}

	public int addNoGoods() {
		int nbNoGoods = 0;

		final int[] levelVariables = this.levelVariables;

		if (levelVariables[0] < 0) {
			return 0;
		}

		final List<Variable> scope = Problem.scope;
		final List<Integer> tuple = Problem.tuple;
		final Set<Integer> pastVariables = Problem.pastVariables;

		// scope.clear();
		tuple.clear();

		scope.add(0, getVariable(levelVariables[0]));
		tuple.add(0, getVariable(levelVariables[0]).getFirstPresentIndex());

		for (int level = 1; level < levelVariables.length; level++) {
			// logger.fine("checking " + getVariable(levelVariables[level-1]));
			pastVariables.add(levelVariables[level - 1]);

			scope.add(level, null);
			tuple.add(level, null);

			for (Variable fv : getVariables()) {
				if (pastVariables.contains(fv.getId())) {
					continue;
				}

				scope.set(level, fv);
				// logger.fine(fv.toString()) ;
				for (int lostIndex = fv.getLastAbsent(); lostIndex >= 0; lostIndex = fv
						.getPrevAbsent(lostIndex)) {
					if (fv.getRemovedLevel(lostIndex) == level) {
						tuple.set(level, lostIndex);

						if (addNoGood()) {
							nbNoGoods++;
						}
					}

				}

			}

			if (levelVariables[level] >= 0) {
				scope.set(level, getVariable(levelVariables[level]));
				tuple.set(level, getVariable(levelVariables[level])
						.getFirstPresentIndex());
			} else {
				break;
			}
		}
		if (logger.isLoggable(Level.FINE)) {
			logger.fine(nbNoGoods + " nogoods");
		}
		scope.clear();
		pastVariables.clear();
		return nbNoGoods;
	}

	public void setLevelVariables(final int level, final int vId) {
		levelVariables[level] = vId;

	}

	public int[] getLevelVariables(final int minLevel) {
		final int[] variables = new int[levelVariables.length - minLevel];
		System.arraycopy(levelVariables, minLevel, variables, 0,
				variables.length);
		return variables;
	}

	public int getMaxArity() {
		return maxArity;
	}

	public void clearNoGoods() {
		for (Constraint c : getConstraints()) {
			c.clearMatrix();
		}
	}

	public static Problem activeProblem(final Problem problem) {
		return activeProblem(problem, 0);
	}

	public static Problem activeProblem(final Problem problem,
			final int additionalConstraints) {
		final Collection<Constraint> constraints = new ArrayList<Constraint>();

		final Collection<Constraint> otherConstraints = new ArrayList<Constraint>();

		final Set<Variable> activeVariables = new TreeSet<Variable>();

		for (Constraint c : problem.getConstraints()) {
			if (c.isActive()) {
				constraints.add(c);
				for (Variable v : c.getInvolvedVariables()) {
					activeVariables.add(v);
				}
				c.setActive(false);
			} else {
				otherConstraints.add(c);
			}

		}

		final Constraint[] sortedConstraints = otherConstraints
				.toArray(new Constraint[otherConstraints.size()]);

		Arrays.sort(sortedConstraints, new cspfj.constraint.Weight(true));

		int i = additionalConstraints;
		for (Constraint c : sortedConstraints) {
			if (i-- <= 0) {
				break;
			}
			constraints.add(c);
			for (Variable v : c.getInvolvedVariables()) {
				activeVariables.add(v);
			}
		}

		return new Problem(activeVariables, constraints, problem.getName());

	}
	
	public String getName() {
		return name ;
	}
}
