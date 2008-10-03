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

package cspfj.constraint.extension;

import java.util.Collection;
import java.util.List;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.problem.Variable;

public class ExtensionConstraint2D extends AbstractPVRConstraint implements
		ExtensionConstraint, DynamicConstraint {

	// private final double tightness;

	protected MatrixManager2D matrix;

	public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
			final boolean shared) {
		this(scope, matrix, shared, false);
	}

	public ExtensionConstraint2D(Variable[] scope, Matrix2D matrix,
			boolean shared, boolean emptyMatrix) {

		super(scope);
		this.matrix = new MatrixManager2D(scope, matrix, shared);
		this.matrix.setTuple(tuple);

		if (emptyMatrix) {
			nbInitConflicts = new long[getArity()][];
			nbMaxConflicts = new long[getArity()];
			for (int i = getArity(); --i >= 0;) {
				nbInitConflicts[i] = new long[scope[i].getDomain().length];
			}
		}

	}

	public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
			final String name, final boolean shared) {
		super(scope, name);

		this.matrix = new MatrixManager2D(scope, matrix, shared);
		this.matrix.setTuple(tuple);
	}

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));

		return matrix.hasSupport(variablePosition, index);
	}

	public boolean revise(final int position, final int level) {
		final Variable variable = getVariable(position);

		assert !variable.isAssigned();

		boolean revised = false;

		for (int index = variable.getFirst(); index >= 0; index = variable
				.getNext(index)) {

			if (!findValidTuple(position, index)) {

				variable.remove(index, level);
				revised = true;
				setActive(true);

			}

		}

		return revised;
	}

	@Override
	public MatrixManager getMatrix() {
		return matrix;
	}

	@Override
	public int removeTuple(int[] tuple) {
		if (matrix.removeTuple(tuple)) {
			addConflict(tuple);
			return 1;
		}
		return 0;
	}

	@Override
	public boolean check() {
		return matrix.check();
	}

	public ExtensionConstraint2D deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraint2D constraint = (ExtensionConstraint2D) super
				.deepCopy(variables);
		constraint.matrix = (MatrixManager2D) matrix.deepCopy(constraint
				.getScope(), constraint.tuple);
		return constraint;
	}

	@Override
	public int removeTuples(int[] fixed, int mobile, int[] values) {
		int removed = 0;
		for (int i : values) {
			fixed[mobile] = i;
			if (removeTuple(fixed) > 0
					&& getVariable(mobile).getRemovedLevel(i) > 0) {
				removed++;
			}
		}
		return removed;
	}

	@Override
	public boolean isSlow() {
		return false;
	}
}
