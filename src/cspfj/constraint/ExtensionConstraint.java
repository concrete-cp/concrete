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

public class ExtensionConstraint extends Constraint {

	// private final double tightness;

	private AbstractMatrixManager matrix;

	private final MatrixManager2D matrix2d;

	public ExtensionConstraint(final Variable[] scope, final Matrix matrix,
			String name) {
		super(scope, name);

		this.matrix = AbstractMatrixManager.factory(scope, matrix);
		this.matrix.setTuple(tuple);

		if (this.matrix instanceof MatrixManager2D) {
			this.matrix2d = (MatrixManager2D) this.matrix;
			this.matrix2d.setLast(last);
		} else {
			matrix2d = null;
		}
	}

	public boolean check() {
		return matrix.check();
	}

	// public double getTightness() {
	// return tightness;
	// }

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getInvolvedVariables()[variablePosition]);

		if (matrix2d != null) {
			if (matrix2d.controlResidue(variablePosition, index)) {
				return true;
			}

			if (!matrix2d.hasSupport(variablePosition, index)) {
				return false;
			}

			matrix2d.updateResidues();

			return true;
		}

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

	public Constraint deepCopy(final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraint constraint = (ExtensionConstraint) super
				.deepCopy(variables);
		constraint.matrix = matrix.deepCopy(constraint.getInvolvedVariables(),
				constraint.tuple);
		return constraint;
	}
	
	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}
