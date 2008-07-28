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

public abstract class AbstractConstraint implements Cloneable, Constraint {
	private int[] positionInVariable;

	public final static int MAX_ARITY = 4;

	private static int cId = 0;

	private Variable[] involvedVariables;

	private final int id;

	private final int arity;

	private static long checks = 0;

	protected static long nbPresenceChecks = 0;

	private static final Logger logger = Logger
			.getLogger("cspfj.constraints.Constraint");

	protected final long[][] nbInitConflicts;

	protected final long[] initSize;

	protected final long[] nbMaxConflicts;

	private final long size;

	private boolean active;

	private final String name;

	protected boolean conflictCounts = false;

	protected int[] tuple;

	protected TupleManager tupleManager;

	public AbstractConstraint(final Variable[] scope) {
		this(scope, null);
	}

	public AbstractConstraint(final Variable[] scope, final String name) {
		involvedVariables = scope.clone();
		arity = involvedVariables.length;
		id = cId++;
		tuple = new int[getArity()];

		tupleManager = new TupleManager(this, tuple);

		nbInitConflicts = new long[arity][];
		for (int i = arity; --i >= 0;) {
			nbInitConflicts[i] = new long[scope[i].getDomain().length];
		}
		nbMaxConflicts = new long[arity];
		initSize = new long[arity];

		size = size();

		active = false;

		positionInVariable = new int[arity];

		if (name == null) {
			this.name = "C" + id;
		} else {
			this.name = name;
		}
	}

	public void initNbSupports() throws InterruptedException {
		// logger.fine("Counting " + this + " supports");
		conflictCounts = false;
		final long size = size();
		for (int p = arity; --p >= 0;) {
			initSize[p] = size / involvedVariables[p].getDomainSize();
		}

		if (Thread.interrupted()) {
			logger.info("Interrupted");
			throw new InterruptedException();
		}
		// logger.fine("Counting supports");

		Arrays.fill(nbMaxConflicts, 0);
		for (int p = arity; --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], 0);
		}

		tupleManager.setFirstTuple();
		do {
			if (Thread.interrupted()) {
				logger.info("Interrupted");
				throw new InterruptedException();
			}
			if (!chk()) {
				for (int p = arity; --p >= 0;) {
					nbInitConflicts[p][tuple[p]]++;
				}
			}
		} while (tupleManager.setNextTuple());
		for (int p = arity; --p >= 0;) {
			final Variable variable = involvedVariables[p];
			long max = 0;
			final long[] nbInitConflicts = this.nbInitConflicts[p];
			for (int i = variable.getFirst(); i != -1; i = variable.getNext(i)) {
				if (max < nbInitConflicts[i]) {
					max = nbInitConflicts[i];
				}
			}
			nbMaxConflicts[p] = max;
		}
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

	public final Variable getVariable(final int position) {
		return involvedVariables[position];
	}

	public final Variable[] getScope() {
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

	protected boolean chk() {
		checks++;
		// logger.info(this + " : " + Arrays.toString(tuple));
		return check();
	}

	public final int getOtherSize(final int position) {
		int size = 1;
		for (int i = arity; --i >= 0;) {
			if (i != position) {
				final int dSize = involvedVariables[i].getDomainSize();
				if (size > Integer.MAX_VALUE / dSize) {
					return Integer.MAX_VALUE;
				}
				size *= dSize;
			}
		}
		return size;
	}

	public boolean supportCondition(final int position) {
		return conflictCounts
				&& getOtherSize(position) > nbMaxConflicts[position];
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

	public final long getNbSupports(final Variable variable, final int index) {
		return getNbSupports(getPosition(variable), index);
	}

	private long getNbSupports(final int position, final int index) {
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

	public boolean revise(final int level, final boolean[] revised,
			final boolean[] removals) {
		return revise(level, revised);
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


	public static final long getPresenceChecks() {
		return nbPresenceChecks;
	}

	public static final void clearStats() {
		checks = nbPresenceChecks = 0;
	}

	public boolean equals(final Object object) {
		if (!(object instanceof Constraint)) {
			return false;
		}
		return id == ((Constraint) object).getId();
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
	public boolean isScope(final Collection<Variable> variables) {
		if (arity != variables.size()) {
			return false;
		}
		for (int i = involvedVariables.length; --i >= 0;) {
			if (!variables.contains(involvedVariables[i])) {
				return false;
			}
		}
		return true;
	}

	public AbstractConstraint deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final AbstractConstraint constraint = this.clone();

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

	public AbstractConstraint clone() throws CloneNotSupportedException {
		final AbstractConstraint constraint = (AbstractConstraint) super
				.clone();

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
		constraint.positionInVariable = new int[arity];
		return constraint;
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

	public String getName() {
		return name;
	}

	public String getType() {
		return this.getClass().getSimpleName();
	}

	public void restore(final int level) {
		// Nothing here
	}

	public int hashCode() {
		return id;
	}
}
