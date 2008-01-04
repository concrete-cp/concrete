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

import cspfj.exception.FailedGenerationException;
import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public class ExtensionConstraint extends Constraint {

	private final double tightness;

	private AbstractMatrixManager matrix;

	public ExtensionConstraint(final Variable[] scope, final boolean supports,
			final int[][] tuples, String name) throws FailedGenerationException {
		super(scope, name);

		final double tight = (double) tuples.length / getSize();

		if (supports) {
			tightness = tight;
		} else {
			tightness = 1 - tight;
		}

		matrix = AbstractMatrixManager.factory(scope, tuple, last);

		try {
			matrix.init(true);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException("Matrix too big");
		}

		intersect(getInvolvedVariables(), supports, tuples);
	}

	public ExtensionConstraint(final Constraint constraint)
			throws FailedGenerationException {
		super(constraint.getInvolvedVariables());

		matrix = AbstractMatrixManager.factory(getInvolvedVariables(), tuple, last);

		try {
			matrix.init(true);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException("Matrix too big");
		}

		long tuples = getSize();

		for (int p = getArity(); --p >= 0;) {
			Arrays.fill(nbInitConflicts[p], 0);
		}

		Arrays.fill(nbMaxConflicts, 0);

		constraint.setFirstTuple();

		do {
			if (!constraint.check()) {
				System.arraycopy(constraint.tuple, 0, tuple, 0, getArity());
				matrix.removeTuple();
				tuples--;
				for (int p = getArity(); --p >= 0;) {
					if (++nbInitConflicts[p][tuple[p]] > nbMaxConflicts[p]) {
						nbMaxConflicts[p] = nbInitConflicts[p][tuple[p]];
					}
				}
			}
		} while (constraint.setNextTuple());

		tightness = 1 - (tuples / getSize());
	}

	public ExtensionConstraint(final Variable[] scope)
			throws FailedGenerationException {
		super(scope);

		tightness = 1;
		for (int p = getArity(); --p >= 0;) {
			final int domainSize = getInvolvedVariables()[p].getDomain().length;

			for (int i = domainSize; --i >= 0;) {
				nbInitConflicts[p][i] = 0;
			}
			nbMaxConflicts[p] = 0;
		}

		matrix = AbstractMatrixManager.factory(scope, tuple, last);

		try {
			matrix.init(true);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException("Matrix too big");
		}
	}
	
	public boolean check() {
		return matrix.check();
	}

	public double getTightness() {
		return tightness;
	}

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getInvolvedVariables()[variablePosition]);

		if (controlResidue(variablePosition, index)) {
			return true;

		}

		if (!matrix.hasSupport(variablePosition, index)) {
			return false;
		}

		updateResidues();

		return true;

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

	public final void intersect(final Variable[] scope, final boolean supports,
			final int[][] tuples) throws FailedGenerationException {
		try {
			matrix.intersect(scope, getInvolvedVariables(), supports, tuples);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException(e.toString());
		}

		initNbSupports();

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

	protected boolean controlResidue(final int position, final int index) {
		return matrix.controlResidue(position, index) ;
	}

	protected void updateResidues() {
		matrix.updateResidues() ;
	}

}
