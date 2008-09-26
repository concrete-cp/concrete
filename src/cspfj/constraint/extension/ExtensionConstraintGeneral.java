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

import java.util.Arrays;
import java.util.Collection;

import cspfj.problem.Variable;

public class ExtensionConstraintGeneral extends AbstractExtensionConstraint {

	// private final static Logger logger = Logger
	// .getLogger(ExtensionConstraintGeneral.class.getSimpleName());

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix, final boolean shared) {
		this(scope, matrix, shared, false);
	}

	public ExtensionConstraintGeneral(final Variable[] scope,
			final Matrix matrix, final String name, final boolean shared) {
		super(scope, new MatrixManager(scope, matrix, shared), name);
	}

	public ExtensionConstraintGeneral(Variable[] scope, Matrix matrix,
			boolean shared, boolean emptyMatrix) {
		super(scope, new MatrixManager(scope, matrix, shared));
		if (emptyMatrix) {
			nbInitConflicts = new long[getArity()][];
			nbMaxConflicts = new long[getArity()];
			for (int i = getArity(); --i >= 0;) {
				nbInitConflicts[i] = new long[scope[i].getDomain().length];
			}
		}
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

	@Override
	public boolean isSlow() {
		return true;
	}

	@Override
	public void initNbSupports() throws InterruptedException {
		if (matrix.getMatrix() instanceof TupleHashSet) {
			final long[][] nbInitConflicts = new long[getArity()][];
			for (int i = getArity(); --i >= 0;) {
				nbInitConflicts[i] = new long[getScope()[i].getDomain().length];
			}
			final long[] nbMaxConflicts = new long[getArity()];

			final long size = currentSize();
			if (size < 0) {
				return;
			}

			final TupleHashSet matrix = (TupleHashSet) this.matrix.getMatrix();
			final boolean initialContent = matrix.getInitialContent();

			Arrays.fill(nbMaxConflicts, 0);
			if (initialContent == false) {
				for (int p = getArity(); --p >= 0;) {
					Arrays.fill(nbInitConflicts[p], size
							/ getVariable(p).getDomainSize());
				}
			} else {
				for (int p = getArity(); --p >= 0;) {
					Arrays.fill(nbInitConflicts[p], 0);
				}
			}

			for (int[] tuple : matrix) {
				if (Thread.interrupted()) {
					throw new InterruptedException();
				}
				if (initialContent == false) {
					for (int p = getArity(); --p >= 0;) {
						nbInitConflicts[p][tuple[p]]--;
					}
				} else {
					for (int p = getArity(); --p >= 0;) {
						nbInitConflicts[p][tuple[p]]++;
					}
				}
			}

			for (int p = getArity(); --p >= 0;) {
				final Variable variable = getVariable(p);
				long max = 0;
				final long[] hereConflicts = nbInitConflicts[p];
				for (int i = variable.getFirst(); i != -1; i = variable
						.getNext(i)) {
					if (max < hereConflicts[i]) {
						max = hereConflicts[i];
					}
				}
				nbMaxConflicts[p] = max;
			}
			this.nbInitConflicts = nbInitConflicts;
			this.nbMaxConflicts = nbMaxConflicts;
		} else {
			super.initNbSupports();
		}
	}

}
