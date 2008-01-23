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
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.constraint.ExtensionConstraint;
import cspfj.constraint.Weight;
import cspfj.exception.FailedGenerationException;
import cspfj.heuristic.Supports;
import cspfj.heuristic.ValueHeuristic;
import cspfj.util.CpuMonitor;

public final class Problem implements Cloneable {
	private Map<Integer, Variable> variables;

	private Variable[] variableArray;

	private Map<Integer, Constraint> constraints;

	private Constraint[] constraintArray;

	private int nbFutureVariables;

	private final static Logger logger = Logger.getLogger("cspfj.Problem");

	private int maxDomainSize;

	private Variable[] levelVariables;

	private int maxArity;

	private String name;

	private boolean useNoGoods;

	private int maxVId;

	private int maxCId;

	public Problem() {
		super();
		this.useNoGoods = true;
	}

	public Problem(Collection<Variable> variables,
			Collection<Constraint> constraints, String name) {
		this();
		setVariables(variables);
		setConstraints(constraints);
		updateInvolvingConstraints();
	}

	public static Problem load(final ProblemGenerator generator)
			throws FailedGenerationException {
		return load(generator, -1);
	}

	public static Problem load(final ProblemGenerator generator,
			final int seconds) throws FailedGenerationException {
		final Problem problem = new Problem();
		Variable.resetVId();
		Constraint.resetCId();

		logger.info("Generating");
		generator.generate();

		logger.info("Setting Variables");
		problem.setVariables(generator.getVariables());

		logger.info("Converting to extension");
		final Collection<Constraint> constraints = convertExtension(generator
				.getConstraints(), seconds);

		logger.info("Setting Constraints");
		problem.setConstraints(constraints);

		logger.info("Updating InvolvingConstraints");
		problem.updateInvolvingConstraints();

		// for (Constraint c :
		// generator.getVariables()[21].getInvolvingConstraints()) {
		// if (c.getInvolvedVariables()[1] == generator.getVariables()[23])
		// System.out.println(c);
		// }

		logger.info("Done");
		return problem;

	}

	public int getNbFutureVariables() {
		return nbFutureVariables;
	}

	private void setVariables(final Collection<Variable> vars) {

		this.variables = new HashMap<Integer, Variable>(vars.size());

		this.variableArray = vars.toArray(new Variable[vars.size()]);

		maxDomainSize = 0;

		maxVId = 0;

		for (Variable var : vars) {
			variables.put(var.getId(), var);
			if (var.getDomain().length > maxDomainSize) {
				maxDomainSize = var.getDomain().length;
			}
			if (var.getId() > maxVId) {
				maxVId = var.getId();
			}
		}

		nbFutureVariables = vars.size();

		levelVariables = new Variable[getNbVariables()];
		Arrays.fill(levelVariables, null);

	}

	private void setConstraints(final Collection<Constraint> cons) {
		this.constraints = new HashMap<Integer, Constraint>(cons.size());

		this.constraintArray = cons.toArray(new Constraint[cons.size()]);

		maxCId = 0;

		for (Constraint c : cons) {
			this.constraints.put(c.getId(), c);
			if (c.getArity() > maxArity) {
				maxArity = c.getArity();
			}
			if (c.getId() > maxCId) {
				maxCId = c.getId();
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
			final Collection<Constraint> involvingConstraints = invConstraints
					.get(v.getId());
			v.setInvolvingConstraints(involvingConstraints
					.toArray(new Constraint[involvingConstraints.size()]));
		}

		// for (Constraint c : getConstraints()) {
		// c.initNbSupports();
		// }

		final ValueHeuristic maxS = new Supports(this, false);
		maxS.compute();
		// for (Variable v: getVariables()) {
		// v.heuristicToOrder();
		// }
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
		// for (AbstractConstraint c : constraints) {
		// c.initLast();
		// }
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		for (Variable v : getVariables()) {
			sb.append(v).append(" : ").append(v.getCurrentDomain())
					.append('\n');
		}

		final SortedSet<Constraint> set = new TreeSet<Constraint>(new Weight(
				false));

		set.addAll(constraints.values());

		for (Constraint c : set) {
			sb.append(c.toString());
			sb.append(' ').append(c.getWeight()).append('\n');
			// sb.append(c.getWeight()).append("\n");
		}
		sb.append('\n');

		return sb.toString();
	}

	public int getMaxDomainSize() {
		return maxDomainSize;
	}

	// public static float noGoodTime = 0;
	//
	// public static float findConstraintTime = 0;
	//
	// public static float removeTupleTime = 0;

	private static <E> int arrayContains(E[] array, int end, E elt) {
		for (int i = end; --i >= 0;) {
			if (array[i] == elt) {
				return i;
			}
		}
		return -1;
	}

	public int addNoGoods() {
		if (!useNoGoods) {
			return 0;
		}

		final float startNoGood = CpuMonitor.getCpuTime();

		int nbNoGoods = 0;

		final Variable[] levelVariables = this.levelVariables;

		if (levelVariables[0] == null) {
			return 0;
		}

		final Variable[] scope = new Variable[levelVariables.length];
		final int[] tuple = new int[levelVariables.length];

		scope[0] = levelVariables[0];
		tuple[0] = scope[0].getFirst();

		for (int level = 1; level < levelVariables.length
				&& level < getMaxArity(); level++) {

			for (Variable fv : variableArray) {
				// logger.fine("checking " +
				// getVariable(levelVariables[level-1]));

				if (arrayContains(scope, level, fv) >= 0) {
					continue;
				}

				// final float startFindConstraint = CpuMonitor.getCpuTime();
				scope[level] = fv;

				final Constraint cons = Constraint.findConstraint(scope,
						level + 1, scope[0].getInvolvingConstraints());

				if (cons == null || !(cons instanceof ExtensionConstraint)) {
					continue;
				}
				final ExtensionConstraint constraint = (ExtensionConstraint) cons;

				final int[] realTuple = constraint.getTuple();
				int currentV = -1;
				for (int i = level + 1; --i >= 0;) {
					if (constraint.getInvolvedVariables()[i] == fv) {
						currentV = i;
					} else {
						realTuple[i] = tuple[arrayContains(scope, level + 1,
								constraint.getInvolvedVariables()[i])];
					}
				}

				// final float startRemoveTuple = CpuMonitor.getCpuTime();
				for (int lostIndex = fv.getLastAbsent(); lostIndex >= 0; lostIndex = fv
						.getPrevAbsent(lostIndex)) {
					if (fv.getRemovedLevel(lostIndex) == level) {
						realTuple[currentV] = lostIndex;

						if (constraint.removeTuple()) {
							// logger.fine(scope + tuple.toString());
							nbNoGoods++;
						}
					}

				}
				// removeTupleTime += CpuMonitor.getCpuTime() -
				// startRemoveTuple;
			}

			if (levelVariables[level] != null) {
				scope[level] = levelVariables[level];
				tuple[level] = levelVariables[level].getFirst();
			} else {
				break;
			}
		}
		// noGoodTime += CpuMonitor.getCpuTime() - startNoGood;
		if (logger.isLoggable(Level.FINE)) {
			logger.fine(nbNoGoods + " nogoods");
		}

		return nbNoGoods;
	}

	public void setLevelVariables(final int level, final Variable variable) {
		levelVariables[level] = variable;

	}

	public Variable getLevelVariable(final int level) {
		return levelVariables[level];
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

	// public void clearNoGoods() {
	// for (Constraint c : getConstraints()) {
	// c.clearNoGoods();
	// }
	// }

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
		return name;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public void setUseNoGoods(final boolean b) {
		this.useNoGoods = b;
	}

	public int getMaxVId() {
		return maxVId;
	}

	public int getMaxCId() {
		return maxCId;
	}

	public float getMaxWeight() {
		float maxWeight = 0;

		for (Constraint c : getConstraints()) {
			if (c.getWeight() > maxWeight) {
				maxWeight = c.getWeight();
			}
		}
		return maxWeight;
	}

	public int getND() {
		int nd = 0;
		for (Variable v : getVariables()) {
			nd += v.getDomainSize();
		}
		return nd;
	}

	public int getMaxFlips() {
		final int nd = getND();
		// return 8 * nd + (int)(.4 * nd * nd);
		// return 5*nd;
		return Math.max((int) (-50000 + 10000 * Math.log(nd)), 10000);
	}

	public int getMaxBacktracks() {
		// final int meanDomainSize = getND() / getNbVariables();
		//
		// final int localBT = getMaxFlips();
		//
		// return (int) (localBT * (100F * getNbVariables()) /
		// (getNbConstraints() * meanDomainSize));
		return Math.max(10, getNbVariables() / 10);
	}

	public Problem clone() throws CloneNotSupportedException {
		final Problem problem = (Problem) super.clone();
		// problem.variables = null;
		//
		// problem.variableArray = null;
		//
		// problem.constraints = null;
		//
		// problem.constraintArray = null;
		//
		// problem.levelVariables = null;

		final Collection<Variable> variables = new ArrayList<Variable>(this
				.getNbVariables());

		for (Variable v : this.getVariables()) {
			variables.add(v.clone());
		}

		problem.setVariables(variables);

		final Collection<Constraint> constraints = new ArrayList<Constraint>(
				this.getNbConstraints());

		for (Constraint c : this.getConstraints()) {
			constraints.add(c.deepCopy(variables));
		}

		problem.setConstraints(constraints);
		problem.updateInvolvingConstraints();
		return problem;
	}

	public static Collection<Constraint> convertExtension(
			final Collection<Constraint> constraints, final int seconds)
			throws FailedGenerationException {
		// for (Constraint c : constraints) {
		// c.initNbSupports();
		// }
		// return constraints;

		final float start = -CpuMonitor.getCpuTime();

		final Collection<Constraint> extConstraints = new ArrayList<Constraint>();

		boolean outOfMemory = false;
		int done = 0;
		for (Constraint c : constraints) {
			if (c instanceof ExtensionConstraint) {
				extConstraints.add(c);
			} else if ((seconds >= 0 && CpuMonitor.getCpuTime() + start > seconds)
					|| outOfMemory) {
				c.initNbSupports();
				extConstraints.add(c);
			} else {
				try {
					extConstraints.add(new ExtensionConstraint(c));
					logger.finer("Converted " + c + ", "
							+ (constraints.size() - ++done) + " remaining");
				} catch (OutOfMemoryError e) {
					outOfMemory = true;
					logger.info("Could not convert " + c + " to extension");
					c.initNbSupports();
					extConstraints.add(c);
				}
			}

		}

		return extConstraints;
	}
}
