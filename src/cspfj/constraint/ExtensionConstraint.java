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

public class ExtensionConstraint extends Constraint {

	private final double tightness;

	private final boolean supports;

	private final int[][] tuples;

	public ExtensionConstraint(final Variable[] scope, final boolean supports,
			final int[][] tuples) throws FailedGenerationException {
		super(scope);

		this.supports = supports;
		this.tuples = tuples;

		final double tight = (double) tuples.length
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

	public double getTightness() {
		return tightness;
	}

	public boolean findValidTuple(final int variablePosition, final int index) {
		assert this.isInvolved(getInvolvedVariables()[variablePosition]);

		if (lastCheck[variablePosition][index]
				&& controlTuplePresence(last[variablePosition][index],
						variablePosition)) {
			return true;

		}

		if (!matrix.setFirstTuple(variablePosition, index)) {
			return false;
		}

		final int arity = getArity();
		final int[] tuple = this.tuple;
		final int[][][] last = this.last;
		final boolean[][] lastCheck = this.lastCheck;

		for (int position = arity; --position >= 0;) {
			final int value = tuple[position];
			// if (last[position][value] == null) {
			// last[position][value] = new int[arity];
			// }
			System.arraycopy(tuple, 0, last[position][value], 0, arity);
			lastCheck[position][value] = true;
		}

		return true;

	}
}
