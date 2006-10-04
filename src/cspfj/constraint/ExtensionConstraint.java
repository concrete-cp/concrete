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

import cspfj.exception.FailedGenerationException;
import cspfj.exception.MatrixTooBigException;
import cspfj.problem.Variable;

public final class ExtensionConstraint extends Constraint {

	private final float tightness;

	private final boolean supports;

	private final int[][] tuples;

	public ExtensionConstraint(final Variable[] scope, final boolean supports,
			final int[][] tuples) throws FailedGenerationException {
		super(scope);

		this.supports = supports;
		this.tuples = tuples;

		final float tight = (float) tuples.length
				/ (scope[0].getDomainSize() * scope[1].getDomainSize());

		if (supports) {
			tightness = tight;
		} else {
			tightness = 1 - tight;
		}

		// try {
		// matrix.init(true);
		// } catch (MatrixTooBigException e) {
		// throw new FailedGenerationException("Matrix too big");
		// }
		try {
			matrix.intersect(getInvolvedVariables(), getInvolvedVariables(),
					supports, tuples);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException(e.toString());
		}
	}

	public ExtensionConstraint(final Variable[] scope)
			throws FailedGenerationException {
		super(scope);

		this.tuples = new int[0][];

		this.supports = false;

		tightness = 1;

		// try {
		// matrix.init(true);
		// } catch (MatrixTooBigException e) {
		// throw new FailedGenerationException("Matrix too big");
		// }

		try {
			matrix.intersect(getInvolvedVariables(), getInvolvedVariables(),
					supports, tuples);
		} catch (MatrixTooBigException e) {
			throw new FailedGenerationException(e.toString());
		}

	}

	public boolean useTupleCache() {
		return true;
	}

	public float getTightness() {
		return tightness;
	}

	protected boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getInvolvedVariables()[variablePosition]);

		final int[] lastTuple = last[variablePosition][index];

		if (lastTuple != null && lastCheck[variablePosition][index]) {
			System.arraycopy(lastTuple, 0, tuple, 0, getArity());

			if (controlTuplePresence(variablePosition)) {
				// System.out.print("c") ;
				return true;
			}

		}

		if (matrix.findTuple(variablePosition, index)) {
			for (int position = getArity(); --position >= 0;) {
				final int value = tuple[position];
				if (last[position][value] == null) {
					last[position][value] = new int[getArity()];
				}
				System.arraycopy(tuple, 0, last[position][value], 0, getArity());
				lastCheck[position][value] = true;
			}
			return true;
		}
		return false;

	}
}
