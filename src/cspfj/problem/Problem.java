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
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.logging.Logger;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.constraint.ExtensionConstraintGeneral;
import cspfj.constraint.Matrix;
import cspfj.constraint.TupleHashSet;
import cspfj.constraint.semantic.RCConstraint;
import cspfj.exception.FailedGenerationException;
import cspfj.util.Waker;

public final class Problem implements Cloneable {
	// private Map<Integer, Variable> variables;

	private Variable[] variableArray;

	private int nbVariables = 0;

	private Map<Integer, Constraint> constraints;

	private Constraint[] constraintArray;

	private int nbFutureVariables = 0;

	private final static Logger logger = Logger.getLogger("cspfj.Problem");

	private int maxDomainSize;

	private Variable[] levelVariables;

	private int maxArity;

	private String name;

	private boolean useNoGoods;

	private int maxVId;

	private int maxCId;

	private int nbNoGoods = 0;

	public Problem() {
		super();
		this.useNoGoods = true;// true;
	}

	public Problem(Collection<Variable> variables,
			Collection<Constraint> constraints, String name) {
		this();
		this.name = name;
		setVariables(variables);
		setConstraints(constraints);
		updateInvolvingConstraints();
	}

	public static Problem load(final ProblemGenerator generator)
			throws FailedGenerationException {
		return load(generator, -1);
	}

	public static Problem load(final ProblemGenerator generator,
			final int expCountSupports) throws FailedGenerationException {
		final Problem problem = new Problem();
		Variable.resetVId();
		AbstractConstraint.resetCId();

		logger.info("Generating");
		generator.generate();

		logger.info("Setting Variables");
		problem.setVariables(generator.getVariables());

		// logger.info("Converting to extension");
		// final Collection<Constraint> constraints = convertExtension(generator
		// .getConstraints(), seconds);

		logger.info("Counting supports (" + expCountSupports + ")");

		if (expCountSupports != 0) {
			Thread.interrupted();
			final Waker waker = new Waker(Thread.currentThread(),
					expCountSupports * 1000);
			waker.start();

			final Constraint[] sorted= generator.getConstraints().toArray(new Constraint[generator.getConstraints().size()]);
			
			Arrays.sort(sorted, new Comparator<Constraint>() {
				@Override
				public int compare(Constraint o1, Constraint o2) {
					return Double.compare(o1.getInitSize(), o2.getInitSize());
				}
			});
			
			for (Constraint c : sorted) {
				try {
					c.initNbSupports();
				} catch (InterruptedException e) {
					break;
				}
			}
			waker.interrupt();
		}

		logger.info("Setting Constraints");
		problem.setConstraints(generator.getConstraints());

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

		// this.variables = new HashMap<Integer, Variable>(vars.size());

		// this.variableArray = vars.toArray(new Variable[vars.size()]);

		maxDomainSize = 0;

		maxVId = 0;

		for (Variable var : vars) {
			if (var.getDomain().length > maxDomainSize) {
				maxDomainSize = var.getDomain().length;
			}
			if (var.getId() > maxVId) {
				maxVId = var.getId();
			}
		}

		this.variableArray = new Variable[maxVId + 1];
		for (Variable var : vars) {
			variableArray[var.getId()] = var;
		}

		nbFutureVariables = nbVariables = vars.size();

		levelVariables = new Variable[getNbVariables()];
		Arrays.fill(levelVariables, null);

	}

	private void setConstraints(final Collection<Constraint> constraints2) {
		this.constraints = new HashMap<Integer, Constraint>(constraints2.size());

		this.constraintArray = constraints2.toArray(new Constraint[constraints2
				.size()]);

		maxCId = 0;

		for (Constraint c : constraints2) {
			// if (c.getArity() == 1) {
			// final Variable variable = c.getInvolvedVariables()[0];
			// final int[] tuple = c.getTuple();
			// for (int i = variable.getFirst(); i >= 0; i = variable
			// .getNext(i)) {
			// tuple[0] = i;
			// if (!c.check()) {
			// variable.remove(i, 0);
			// }
			//
			// }
			// continue ;
			// }
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
				variableArray.length);

		for (Variable v : getVariables()) {
			invConstraints.put(v.getId(), new ArrayList<Constraint>());
		}

		for (Constraint c : getConstraints()) {
			for (Variable v : c.getScope()) {
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

		// final ValueHeuristic maxS = new Supports(this, false);
		// maxS.compute();

		// for (Variable v: getVariables()) {
		// v.heuristicToOrder();
		// }
	}

	public int getNbVariables() {
		return nbVariables;
	}

	public int getNbConstraints() {
		return constraints.size();
	}

	public Constraint[] getConstraints() {
		return constraintArray;
	}

	public Collection<Constraint> getConstraintsAsCollection() {
		return constraints.values();
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
		return variableArray[vId];
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
		// logger.fine("Restoring to " + level);

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
		for (Constraint c : getConstraints()) {
			c.restore(level);
		}

	}

	public void restoreAll(final int level) {
		// logger.fine("Restoring all to " + level);

		for (Variable v : getVariables()) {
			if (v.isAssigned()) {
				v.unassign(this);
			}

			for (int i = level; i < getNbVariables(); i++) {
				v.restoreLevel(i);
			}

		}
		// for (AbstractConstraint c : constraints) {
		// c.initLast();
		// }
		for (int i = level; i < getNbVariables(); i++) {
			for (Constraint c : getConstraints()) {
				c.restore(i);
			}
		}
	}

	@Override
	public String toString() {
		final StringBuffer sb = new StringBuffer();
		for (Variable v : getVariables()) {
			sb.append(v).append(" : ").append(v.getCurrentDomain())
					.append('\n');
		}

		for (Constraint c : constraintArray) {
			sb.append(c.toString());
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

	private static <E> int arrayContains(final E[] array, final int end,
			final E elt) {
		for (int i = end; --i >= 0;) {
			if (array[i] == elt) {
				return i;
			}
		}
		return -1;
	}

	private final static DynamicConstraint findDynamicConstraint(
			final Variable[] scope, final int stopScope) {

		for (Constraint c : scope[0].getInvolvingConstraints()) {
			if (c.getArity() != stopScope || !(c instanceof DynamicConstraint)) {
				continue;
			}
			boolean ok = true;
			for (int i = stopScope; --i >= 1;) {
				if (!c.isInvolved(scope[i])) {
					ok = false;
					break;
				}
			}
			if (ok) {
				return (DynamicConstraint) c;
			}
		}
		return null;
	}

	public Set<Constraint> addNoGoods(final boolean addConstraints) {
		final Set<Constraint> modifiedConstraints = new HashSet<Constraint>();

		if (!useNoGoods) {
			return modifiedConstraints;
		}

		// final float startNoGood = CpuMonitor.getCpuTime();

		// int nbNoGoods = 0;

		final Variable[] levelVariables = this.levelVariables;

		if (levelVariables[0] == null) {
			return modifiedConstraints;
		}

		final Variable[] scope = new Variable[levelVariables.length];
		final int[] tuple = new int[levelVariables.length];

		scope[0] = levelVariables[0];
		tuple[0] = scope[0].getFirst();

		for (int level = 1; level < levelVariables.length; level++) {
			final int[] realTuple = new int[level + 1];
			for (Variable fv : variableArray) {
				// logger.fine("checking " +
				// getVariable(levelVariables[level-1]));

				if (arrayContains(scope, level, fv) >= 0) {
					continue;
				}

				int firstLost = fv.getLastAbsent();
				if (firstLost < 0) {
					continue;
				}

				// final float startFindConstraint = CpuMonitor.getCpuTime();
				scope[level] = fv;

				DynamicConstraint constraint = findDynamicConstraint(scope,
						level + 1);

				if (constraint == null) {
					if (!addConstraints) {
						continue;
					}
					while (firstLost >= 0 && fv.getRemovedLevel(firstLost) != level) {
						firstLost = fv.getPrevAbsent(firstLost);
					}
					if (firstLost < 0) {
						continue;
					}
					
					final Variable[] constraintScope = new Variable[level + 1];
					System.arraycopy(scope, 0, constraintScope, 0,
							constraintScope.length);

					if (level == 1) {
//						final Matrix2D matrix = new Matrix2D(scope[0]
//								.getDomain().length,
//								scope[1].getDomain().length, true);
//						constraint = new ExtensionConstraint2D(constraintScope,
//								matrix, false);
						constraint = new RCConstraint(constraintScope);
					} else {
						final Matrix matrix = new TupleHashSet(true);
						try {
							constraint = new ExtensionConstraintGeneral(
									constraintScope, matrix, false);
						} catch (FailedGenerationException e) {
							logger.info("Could not generate constraint over "
									+ Arrays.toString(constraintScope));
							continue;
						}
					}

				}

				int currentV = -1;
				for (int i = level + 1; --i >= 0;) {
					if (constraint.getVariable(i) == fv) {
						currentV = i;
					} else {
						realTuple[i] = tuple[arrayContains(scope, level + 1,
								constraint.getVariable(i))];
					}
				}

				// final float startRemoveTuple = CpuMonitor.getCpuTime();
				for (int lostIndex = firstLost; lostIndex >= 0; lostIndex = fv
						.getPrevAbsent(lostIndex)) {
					if (fv.getRemovedLevel(lostIndex) == level) {
						realTuple[currentV] = lostIndex;

						if (constraint.removeTuple(realTuple)) {
							// logger.fine(scope + tuple.toString());
							nbNoGoods++;
							modifiedConstraints.add(constraint);
						}
					}

				}
				// removeTupleTime += CpuMonitor.getCpuTime() -
				// startRemoveTuple;
			}

			if (levelVariables[level] == null) {
				break;

			}
			scope[level] = levelVariables[level];
			tuple[level] = levelVariables[level].getFirst();

		}

		// noGoodTime += CpuMonitor.getCpuTime() - startNoGood;
		// if (logger.isLoggable(Level.INFO)) {
		if (modifiedConstraints.size() > 0) {
			logger.fine(nbNoGoods + " nogoods");
		}
		// }
		final Collection<Constraint> curCons = new ArrayList<Constraint>(
				constraints.values());

		boolean added = false;
		for (Constraint c : modifiedConstraints) {
			if (!curCons.contains(c)) {
				curCons.add(c);
				added = true;
			}
		}
		if (added) {
			setConstraints(curCons);
			updateInvolvingConstraints();
			logger.info(getNbConstraints() + " constraints, " + getMaxArity()
					+ " max arity");
		}

		return modifiedConstraints;
	}

	public int getNbNoGoods() {
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
				for (Variable v : c.getScope()) {
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
			for (Variable v : c.getScope()) {
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
		return Math.max(10, maxDomainSize / 10);
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

	// public static Collection<Constraint> convertExtension(
	// final Collection<Constraint> constraints, final int seconds)
	// throws FailedGenerationException {
	// // for (Constraint c : constraints) {
	// // c.initNbSupports();
	// // }
	// // return constraints;
	//
	// final float start = -CpuMonitor.getCpuTime();
	//
	// final Collection<Constraint> extConstraints = new
	// ArrayList<Constraint>();
	//
	// boolean outOfMemory = false;
	// int done = 0;
	// for (Constraint c : constraints) {
	// if (c instanceof ExtensionConstraint) {
	// extConstraints.add(c);
	// } else if ((seconds >= 0 && CpuMonitor.getCpuTime() + start > seconds)
	// || outOfMemory) {
	// c.initNbSupports();
	// extConstraints.add(c);
	// } else {
	// try {
	// extConstraints.add(new ExtensionConstraint(c));
	// logger.finer("Converted " + c + ", "
	// + (constraints.size() - ++done) + " remaining");
	// } catch (OutOfMemoryError e) {
	// outOfMemory = true;
	// logger.info("Could not convert " + c + " to extension");
	// c.initNbSupports();
	// extConstraints.add(c);
	// }
	// }
	//
	// }
	//
	// return extConstraints;
	// }
}
