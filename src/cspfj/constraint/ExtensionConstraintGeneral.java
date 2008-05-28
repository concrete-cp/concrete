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

import cspfj.problem.Variable;

public class ExtensionConstraintGeneral extends AbstractExtensionConstraint {

	private final TupleListSorted sorted;

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix) {
		super(scope, new MatrixManagerGeneral(scope, matrix));

		if (matrix instanceof TupleListSorted) {
			final TupleListSorted sorted = (TupleListSorted) matrix;

			if (sorted.getInitialContent() == false) {
				this.sorted = sorted;
			} else {
				this.sorted = null;
			}

		} else {
			sorted = null;
		}
	}

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix, String name) {
		super(scope, new MatrixManagerGeneral(scope, matrix), name);
		if (matrix instanceof TupleListSorted) {
			final TupleListSorted sorted = (TupleListSorted) matrix;

			if (sorted.getInitialContent() == false) {
				this.sorted = sorted;
			} else {
				this.sorted = null;
			}

		} else {
			sorted = null;
		}
	}

	@Override
	public boolean findValidTuple(int variablePosition, int index) {
		if (sorted == null) {
			return super.findValidTuple(variablePosition, index);
		}

		final boolean residue = last[variablePosition][index][0] != -1;

		if (residue
				&& controlTuplePresence(last[variablePosition][index],
						variablePosition)) {
			return true;
		}

		tuple[variablePosition] = index;
		
		int[] currentAllowedTuple = sorted.first();
		
		while (currentAllowedTuple != null
				&& tupleManager.setTupleAfter(currentAllowedTuple,
						variablePosition)) {
			if (Arrays.equals(currentAllowedTuple, tuple)) {
				return true;
			}
			currentAllowedTuple = sorted.higher(tuple);
		}

		return false;

	}

	public ExtensionConstraintGeneral deepCopy(
			final Collection<Variable> variables)
			throws CloneNotSupportedException {
		final ExtensionConstraintGeneral constraint = (ExtensionConstraintGeneral) super
				.deepCopy(variables);
		constraint.matrix = matrix.deepCopy(constraint.getScope(),
				constraint.tuple);
		return constraint;
	}

	public String getType() {
		return super.getType() + " w/ " + matrix.getType();
	}
}
