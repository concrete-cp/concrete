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
import cspfj.problem.Variable;

public final class ExtensionConstraint extends Constraint {

	private final float tightness;

	public ExtensionConstraint(final Variable[] scope, final boolean supports, final int[][] tuples)
			throws FailedGenerationException {
		super(scope);

		initMatrix(!supports);

		final float tight = (float) tuples.length
				/ (scope[0].getDomainSize() * scope[1].getDomainSize());

		if (supports) {
			tightness = tight;
		} else {
			tightness = 1 - tight;
		}

		int[] indexes = new int[getArity()];

		for (int[] tuple : tuples) {
			for (int i = 0; i < tuple.length; i++) {
				indexes[i] = scope[i].index(tuple[i]);
			}

			setMatrixIndex(indexes, supports) ;

		}

	}

	public ExtensionConstraint(final Variable[] scope) {
		super(scope);

        initMatrix(true);

        tightness = 1;

	}



	public boolean useTupleCache() {
		return true;
	}

	public float getTightness() {
		return tightness;
	}

	// public final int getNbTuples(final Variable variable, final int index) {
	// int nbTuples = 0;
	// if (getPosition(variable) == 0) {
	// for (int i = 0; i < getInvolvedVariables()[1].getDomain().length; i++) {
	// if (matrix[index][i]) {
	// nbTuples++;
	// }
	// }
	// } else {
	// for (int i = 0; i < getInvolvedVariables()[0].getDomain().length; i++) {
	// if (matrix[i][index]) {
	// nbTuples++;
	// }
	// }
	// }
	// return nbTuples;
	// }


}
