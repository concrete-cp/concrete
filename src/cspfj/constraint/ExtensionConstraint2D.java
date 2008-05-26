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

public class ExtensionConstraint2D extends AbstractExtensionConstraint {

	// private final double tightness;

	protected MatrixManager2D matrix;

	public ExtensionConstraint2D(final Variable[] scope, final Matrix2D matrix,
			final String name) {
		super(scope, new MatrixManager2D(scope, matrix), name);

		this.matrix = (MatrixManager2D) super.matrix;
		this.matrix.setLast(last);
	}



	// public double getTightness() {
	// return tightness;
	// }

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));

		if (matrix.controlResidue(variablePosition, index)) {
			return true;
		}

		if (!matrix.hasSupport(variablePosition, index)) {
			return false;
		}

		matrix.updateResidues();

		return true;
	}



	public ExtensionConstraint2D deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraint2D constraint = (ExtensionConstraint2D) super
				.deepCopy(variables);
		constraint.matrix = (MatrixManager2D) matrix.deepCopy(constraint
				.getScope(), constraint.tuple);
		return constraint;
	}

	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}