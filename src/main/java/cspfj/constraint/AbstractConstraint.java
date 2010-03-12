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
import java.util.HashSet;
import java.util.Set;

import cspfj.constraint.extension.TupleManager;
import cspfj.problem.AbstractVariable;
import cspfj.problem.IntVariable;

public abstract class AbstractConstraint implements Cloneable, Constraint {
	private static int cId = 0;

	private static long checks = 0;

	private static long nbPresenceChecks = 0;

	public static final long getPresenceChecks() {
		return nbPresenceChecks;
	}

	public static final void clearStats() {
		checks = nbPresenceChecks = 0;
	}

	public static final long getChecks() {
		return checks;
	}

	private AbstractVariable[] scope;

	private Set<AbstractVariable> scopeSet;

	private final int id;

	private final int arity;

	private boolean active;

	private final String name;

	protected int[] tuple;

	private int weight = 1;

	private final int[] removals;

	public AbstractConstraint(final IntVariable... scope) {
		this(null, scope);
	}

	public AbstractConstraint(final String name,
			final AbstractVariable... scope) {
		this.scope = scope.clone();
		scopeSet = new HashSet<AbstractVariable>(scope.length, 1);
		for (AbstractVariable v : scope) {
			scopeSet.add(v);
		}
		arity = scope.length;
		id = cId++;
		tuple = new int[arity];

		active = false;

		if (name == null) {
			this.name = "C" + id;
		} else {
			this.name = name;
		}

		removals = new int[arity];// new boolean[arity];
	}

	public int getRemovals(int position) {
		return removals[position];
	}

	public void setRemovals(int position, int value) {
		removals[position] = value;
	}

	public void fillRemovals(final int value) {
		Arrays.fill(removals, value);
	}

	public boolean hasNoRemovals(final int value) {
		for (int i : removals) {
			if (i >= value) {
				return false;
			}
		}
		return true;
	}

	@Override
	public int getWeight() {
		return weight;
	}

	@Override
	public void incWeight() {
		weight++;
	}

	@Override
	public void setWeight(final int weight) {
		this.weight = weight;
	}

	public int getValue(final int position) {
		return scope[position].getValue(tuple[position]);
	}

	public final boolean isInvolved(final AbstractVariable variable) {
		return getPosition(variable) >= 0;
	}

	public final int getPosition(final AbstractVariable variable) {
		for (int i = arity; --i >= 0;) {
			if (scope[i] == variable) {
				return i;
			}
		}

		return -1;
	}

	public final int[] getTuple() {
		return tuple;
	}

	public AbstractVariable getVariable(final int position) {
		return scope[position];
	}

	public AbstractVariable[] getScope() {
		return scope;
	}

	public final Set<AbstractVariable> getScopeSet() {
		return scopeSet;
	}

	public final int getId() {
		return id;
	}

	public final static void resetCId() {
		cId = 0;
	}

	public String toString() {
		return name + " " + Arrays.toString(scope);
	}

	protected boolean chk() {
		checks++;
		// logger.info(this + " : " + Arrays.toString(tuple));
		return check();
	}

	protected boolean controlTuplePresence(final int[] tuple, final int position) {
		nbPresenceChecks++;
		final AbstractVariable[] involvedVariables = this.scope;
		for (int i = arity; --i >= 0;) {
			if (i != position && !involvedVariables[i].isPresent(tuple[i])) {
				return false;
			}
		}

		return true;
	}

	public final boolean isBound(IntVariable variable) {
		for (AbstractVariable v : scope) {
			if (v != variable && v.getDomainSize() > 1) {
				return true;
			}
		}
		return false;
	}

	//
	// public final boolean check(int[] tuple) {
	// System.arraycopy(tuple, 0, this.tuple, 0, arity);
	// return chk();
	// }
	//
	// public final boolean checkFirstWith(final int variablePosition,
	// final int index) {
	// tupleManager.setFirstTuple(variablePosition, index);
	// return chk();
	// }

	public final int getArity() {
		return arity;
	}

	public final boolean isActive() {
		return active;
	}

	public final void setActive(final boolean active) {
		this.active = active;
	}

	public boolean equals(final Object object) {
		if (!(object instanceof Constraint)) {
			return false;
		}
		return id == ((Constraint) object).getId();
	}

	public AbstractConstraint deepCopy(final Collection<IntVariable> variables)
			throws CloneNotSupportedException {
		final AbstractConstraint constraint = this.clone();

		constraint.scope = new IntVariable[arity];

		for (int i = arity; --i >= 0;) {
			for (IntVariable v : variables) {
				if (v == scope[i]) {
					constraint.scope[i] = v;
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
		return constraint;
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

	public void setLevel(final int level) {
		// Nothing here
	}

	public int hashCode() {
		return id;
	}

	public int getEvaluation(int reviseCount) {
		return Integer.MAX_VALUE;
	}
}
