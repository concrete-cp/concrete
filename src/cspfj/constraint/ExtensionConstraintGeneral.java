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
import java.util.List;

import cspfj.problem.Variable;

public class ExtensionConstraintGeneral extends AbstractConstraint implements ExtensionConstraint {

	// private final double tightness;

	private MatrixManagerGeneral matrix;

	public ExtensionConstraintGeneral(final Variable[] scope, final Matrix matrix,
			String name) {
		super(scope, name);

		this.matrix = new MatrixManagerGeneral(scope, matrix);
		this.matrix.setTuple(tuple);
	}

	public boolean check() {
		return matrix.check();
	}

	// public double getTightness() {
	// return tightness;
	// }

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getVariable(variablePosition));

		return super.findValidTuple(variablePosition, index);

	}

	public boolean removeTuple(final List<Variable> scope,
			final List<Integer> scrambledTuple) {

		tupleManager.setRealTuple(scope, scrambledTuple);

		if (matrix.removeTuple()) {
			for (int p = getArity(); --p >= 0;) {
				if (Arrays.equals(tuple, last[p][tuple[p]])) {
					Arrays.fill(last[p][tuple[p]], -1);
				}

				if (++nbInitConflicts[p][tuple[p]] > nbMaxConflicts[p]) {
					nbMaxConflicts[p] = nbInitConflicts[p][tuple[p]];
				}
			}
			return true;
		}

		return false;

	}

	public boolean removeTuple() {
		return matrix.removeTuple();
	}

	public AbstractMatrixManager getMatrix() {
		return matrix;
	}

	public ExtensionConstraintGeneral deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraintGeneral constraint = (ExtensionConstraintGeneral) super
				.deepCopy(variables);
		constraint.matrix = (MatrixManagerGeneral) matrix.deepCopy(constraint.getScope(),
				constraint.tuple);
		return constraint;
	}
	
	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}
