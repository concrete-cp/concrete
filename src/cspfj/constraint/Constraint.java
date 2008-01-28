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

package cspfj.constraint;

import java.util.Arrays;
import java.util.Collection;
import java.util.logging.Logger;

import cspfj.problem.Variable;
import cspfj.util.CpuMonitor;

public abstract class Constraint implements Comparable<Constraint>, Cloneable {
	private Variable[] involvedVariables;

	private int[] positionInVariable;

	public final static int MAX_ARITY = 4;

	protected int[][][] last;

	private static int cId = 0;

	private final int id;

	protected int[] tuple;

	private int weight = 1;

	private final int arity;

	private static long checks = 0;

	protected static long nbPresenceChecks = 0;

	protected static final Logger logger = Logger
			.getLogger("cspfj.constraints.Constraint");

	private boolean tupleCache;

	protected TupleManager tupleManager;

	protected final long[][] nbInitConflicts;

	protected final long[] initSize;

	protected final long[] nbMaxConflicts;

	private final long size;

	private boolean active;

	private final String name;

	private boolean conflictCounts = false;
	
	protected Constraint(final Variable[] scope) {
		this(scope, true, null);
	}

	protected Constraint(final Variable[] scope, final String name) {
		this(scope, true, name);
	}

	protected Constraint(final Variable[] scope, final boolean tupleCache) {
		this(scope, tupleCache, null);
	}

	protected Constraint(final Variable[] scope, final boolean tupleCache,
			final String name) {
		involvedVariables = scope.clone();
		arity = involvedVariables.length;

		tuple = new int[arity];

		id = cId++;

		int maxDomain = 0;
		for (int i = arity; --i >= 0;) {
			if (involvedVariables[i].getDomain().length > maxDomain) {
				maxDomain = involvedVariables[i].getDomain().length;
			}
		}

		this.tupleCache = tupleCache;

		if (tupleCache) {
			last = new int[arity][maxDomain][arity];
			for (int i = arity; --i >= 0;) {
				for (int j = maxDomain; --j >= 0;) {
					Arrays.fill(last[i][j], -1);
				}
			}
		}

		nbInitConflicts = new long[arity][maxDomain];
		nbMaxConflicts = new long[arity];
		initSize = new long[arity];

		size = size();

		active = false;

		positionInVariable = new int[arity];

		tupleManager = new TupleManager(this, tuple);

		if (name == null) {
			this.name = "C" + id;
		} else {
			this.name = name;
		}
	}

	public void removeTupleCache() {
		tupleCache = false;
		last = null;
	}

	public boolean isCachingTuples() {
		return tupleCache;
	}

	public void initNbSupports(final float time) {
		// logger.info("Counting " + this +" supports");

		final long size = size();
		for (int p = arity; --p >= 0;) {
			initSize[p] = size / involvedVariables[p].getDomainSize();
		}

		// logger.fine("Counting supports");

		tupleManager.setFirstTuple();

		Arrays.fill(nbMaxConflicts, 0);
		for (int p = arity; --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], 0);
		}
		do {
			if (!chk()) {
				for (int p = arity; --p >= 0;) {
					final long i = ++nbInitConflicts[p][tuple[p]];
					if (i > nbMaxConflicts[p]) {
						nbMaxConflicts[p] = i;
					}
				}
			}
			if (CpuMonitor.getCpuTime() > time) {
				return;
			}
		} while (tupleManager.setNextTuple());
		conflictCounts = true;

	}

	public int getValue(final int position) {
		return involvedVariables[position].getDomain()[tuple[position]];
	}

	public final boolean isInvolved(final Variable variable) {
		return getPosition(variable) >= 0;
	}

	public final int getPosition(final Variable variable) {
		for (int i = arity; --i >= 0;) {
			if (involvedVariables[i] == variable) {
				return i;
			}
		}

		return -1;
	}

	public final int getPositionInVariable(final Variable variable) {
		return getPositionInVariable(getPosition(variable));
	}

	public final int getPositionInVariable(final int position) {
		return positionInVariable[position];
	}

	public final void setPositionInVariable(final int positionInConstraint,
			final int positionInVariable) {
		this.positionInVariable[positionInConstraint] = positionInVariable;
	}

	public final int[] getTuple() {
		return tuple;
	}

	public final Variable[] getInvolvedVariables() {
		return involvedVariables;
	}

	public final int getId() {
		return id;
	}

	public final static void resetCId() {
		cId = 0;
	}

	public String toString() {
		return name + " " + Arrays.toString(involvedVariables);
	}

	private boolean chk() {
		checks++;
		return check();
	}

	public abstract boolean check();

	public final int getOtherSize(final int position) {
		if (arity > MAX_ARITY) {
			return -1;
		}
		int size = 1;
		for (int i = arity; --i >= 0;) {
			if (i != position) {
				size *= involvedVariables[i].getDomainSize();
			}
		}
		return size;
	}

	public boolean skipRevision(final int position) {
		return conflictCounts && getOtherSize(position) > nbMaxConflicts[position];
	}
	
	public boolean revise(final int position, final int level) {
		final Variable variable = involvedVariables[position];

		assert !variable.isAssigned();

		boolean revised = false;

		// logger.finer("Revising " + variable + " "
		// + Arrays.toString(variable.getCurrentDomain()) + " against "
		// + this);

		

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {

			// logger.finer("Checking (" + variable + ", " + index + ")");
//			if (conflictCounts && othersSize > nbInitConflicts[position][index]) {
//				continue;
//			}
			if (!findValidTuple(position, index)) {
				// logger.finer("removing " + index + " from " + variable);

				variable.remove(index, level);

				revised = true;
				active = true;

			}

		}

		return revised;
	}

	protected boolean controlTuplePresence(final int[] tuple, final int position) {
		nbPresenceChecks++;
		final Variable[] involvedVariables = this.involvedVariables;
		for (int i = arity; --i >= 0;) {
			if (i != position && !involvedVariables[i].isPresent(tuple[i])) {
				return false;
			}
		}

		return true;
	}

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(involvedVariables[variablePosition]);

		if (controlResidue(variablePosition, index)) {
			return true;
		}

		tupleManager.setFirstTuple(variablePosition, index);

		do {
			if (chk()) {
				updateResidues();
				return true;
			}
		} while (tupleManager.setNextTuple(variablePosition));

		return false;

	}

	public int nbTuples(final int variablePosition, final int index) {
		if (arity > MAX_ARITY) {
			return 0;
		}

		tupleManager.setFirstTuple(variablePosition, index);

		int nbTuples = 0;

		do {
			if (chk()) {
				nbTuples++;
			}
		} while (tupleManager.setNextTuple(variablePosition));

		return nbTuples;
	}

	public final long getNbSupports(final Variable variable, final int index) {
		return getNbSupports(getPosition(variable), index);
	}

	public final long getNbSupports(final int position, final int index) {
		if (!conflictCounts) {
			return -1;
		}
		return (size / involvedVariables[position].getDomain().length)
				- nbInitConflicts[position][index];
	}

	public final long getNbInitConflicts(final int position, final int index) {
		if (!conflictCounts) {
			return -1;
		}
		return nbInitConflicts[position][index];
	}

	public final long getNbMaxConflicts(final int position) {
		if (!conflictCounts) {
			return -1;
		}
		return nbMaxConflicts[position];
	}

	public final void increaseWeight() {
		weight++;
	}

	public final void increaseWeight(final int weight) {
		this.weight += weight;
	}

	public final int getWeight() {
		return weight;
	}

	public final void setWeight(final int weight) {
		this.weight = weight;
	}

	public final boolean isBound() {
		boolean bound = false;
		for (Variable v : involvedVariables) {
			if (v.getDomainSize() > 1) {
				if (bound) {
					return true;
				}
				bound = true;
			}
		}
		return false;
	}

	public static final long getChecks() {
		return checks;
	}

	public final boolean checkFirst() {
		tupleManager.setFirstTuple();
		return chk();
	}

	public final boolean checkFirstWith(final int variablePosition,
			final int index) {
		tupleManager.setFirstTuple(variablePosition, index);
		return chk();
	}

	public final int getArity() {
		return arity;
	}

	public final boolean isActive() {
		return active;
	}

	public final void setActive(final boolean active) {
		this.active = active;
	}

	public final static Constraint findConstraint(final Variable[] scope,
			final Collection<Constraint> constraints) {
		return findConstraint(scope, scope.length, constraints
				.toArray(new Constraint[constraints.size()]));
	}

	public final static Constraint findConstraint(final Variable[] scope,
			final int stopScope, final Constraint[] constraints) {

		for (Constraint c : constraints) {
			if (c.getArity() != stopScope) {
				continue;
			}
			boolean ok = true;
			for (int i = stopScope; --i >= 0;) {
				if (!c.isInvolved(scope[i])) {
					ok = false;
					break;
				}
			}
			if (ok) {
				return c;
			}
		}
		return null;
	}

	public static final long getPresenceChecks() {
		return nbPresenceChecks;
	}

	public static final void clearStats() {
		checks = nbPresenceChecks = 0;
	}

	public boolean equalsConstraint(final Constraint constraint) {
		return id == constraint.getId();
	}

	/**
	 * Determines if the given set of variables corresponds to the set of
	 * variables involved in the constraint.
	 * 
	 * @param variables
	 *            a given set of variables
	 * @return <code> true </code> iff the given set of variables corresponds to
	 *         the set of variables involved in the constraint
	 */
	public boolean hasSetOfVariablesEqualTo(final Collection<Variable> variables) {
		if (arity != variables.size()) {
			return false;
		}
		for (int i = involvedVariables.length; --i >= 0;) {
			boolean found = false;
			for (Variable v : variables) {
				if (involvedVariables[i] == v) {
					found = true;
					break;
				}
			}
			if (!found) {
				return false;
			}
		}
		return true;
	}

	public int compareTo(final Constraint constraint) {
		return constraint.getId() - id;
	}

	public Constraint deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final Constraint constraint = this.clone();

		constraint.involvedVariables = new Variable[arity];

		for (int i = arity; --i >= 0;) {
			for (Variable v : variables) {
				if (v == involvedVariables[i]) {
					constraint.involvedVariables[i] = v;
					break;
				}
			}
		}

		return constraint;
	}

	public Constraint clone() throws CloneNotSupportedException {
		final Constraint constraint = (Constraint) super.clone();

		constraint.tuple = new int[arity];

		// constraint.positionInVariable fixe
		// constraint.nbMaxConflicts fixe
		// constraint.nbSupports fixe

		constraint.tupleManager = new TupleManager(constraint, constraint.tuple);

		int maxDomain = 0;
		for (int i = arity; --i >= 0;) {
			if (involvedVariables[i].getDomain().length > maxDomain) {
				maxDomain = involvedVariables[i].getDomain().length;
			}
		}

		if (tupleCache) {
			constraint.last = new int[arity][maxDomain][arity];
			for (int i = arity; --i >= 0;) {
				for (int j = maxDomain; --j >= 0;) {
					Arrays.fill(constraint.last[i][j], -1);
				}
			}

		}

		constraint.positionInVariable = new int[arity];
		return constraint;
	}

	public void setFirstTuple() {
		tupleManager.setFirstTuple();
	}

	public boolean setNextTuple() {
		return tupleManager.setNextTuple();
	}

	public boolean storeNoGoods() {
		return false;
	}

	public long getSize() {
		return size;
	}

	protected final long size() {
		long size = 1;
		for (Variable v : involvedVariables) {
			size *= v.getDomainSize();
		}
		return size;
	}

	protected boolean controlResidue(final int position, final int index) {
		if (tupleCache && last[position][index][0] != -1
				&& controlTuplePresence(last[position][index], position)) {
			return true;
		}
		return false;
	}

	protected void updateResidues() {
		if (tupleCache) {
			for (int position = arity; --position >= 0;) {
				final int value = tuple[position];
				System.arraycopy(tuple, 0, last[position][value], 0, arity);
			}
		}
	}

	public String getName() {
		return name;
	}
}
