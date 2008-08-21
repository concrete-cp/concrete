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

import java.util.Collection;

import cspfj.problem.Variable;

public class ExtensionConstraint2D extends AbstractPVRConstraint implements
		ExtensionConstraint, DynamicConstraint {

	// private final double tightness;

	protected MatrixManager2D matrix;

	public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
			final boolean shared) {
		super(scope);

		this.matrix = new MatrixManager2D(scope, matrix, shared);
		this.matrix.setTuple(tuple);
	}

	public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
			final String name, final boolean shared) {
		super(scope, name);

		this.matrix = new MatrixManager2D(scope, matrix, shared);
		this.matrix.setTuple(tuple);
	}

	// public double getTightness() {
	// return tightness;
	// }

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
	public boolean removeTuple(int[] tuple) {
		return matrix.removeTuple(tuple);
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
	public boolean isSlow() {
		return false;
	}
}
