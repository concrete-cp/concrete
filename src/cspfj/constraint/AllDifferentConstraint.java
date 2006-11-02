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
import java.util.List;

import cspfj.problem.Variable;

public final class AllDifferentConstraint extends Constraint {

	private final boolean[] union;

	private final int min;

	// private final Integer[] constants;

//	private final static Logger logger = Logger
//			.getLogger("cspfj.constraint.AllDifferentConstraint");

	// private boolean removedConstants = false;

	public AllDifferentConstraint(final Variable[] scope) {
		super(scope);

		int minVal = Integer.MAX_VALUE;
		int maxVal = Integer.MIN_VALUE;
		for (Variable v : scope) {
			for (int i : v.getDomain()) {
				if (i > maxVal) {
					maxVal = i;
				}
				if (i < minVal) {
					minVal = i;
				}
			}
		}
		union = new boolean[maxVal - minVal + 1];
		min = minVal;
	}

	@Override
	public boolean check() {
		final boolean[] union = this.union;
		Arrays.fill(union, false);
		final int min = this.min ;
		final Variable[] involvedVariables = getInvolvedVariables();
		final int[] tuple = this.tuple;
		for (int i = getArity(); --i >= 0;) {
			final int index = involvedVariables[i].getDomain()[tuple[i]];
			if (union[index - min]) {
				return false;
			}
			union[index - min] = true;
		}
		return true;
	}

	public boolean revise(final int position, final int level) {
		final Variable variable = getInvolvedVariables()[position];
		assert !variable.isAssigned();

		final boolean[] union = this.union;
		final int min = this.min ;
		
		Arrays.fill(union, false);
		int nbVal = 0;

		boolean revised = false;

		for (int checkPos = getArity(); --checkPos >= 0;) {
			final Variable checkedVariable = getInvolvedVariables()[checkPos];
			for (int i = checkedVariable.getFirst(); i >= 0; i = checkedVariable
					.getNext(i)) {

				if (!union[checkedVariable.getDomain()[i] - min]) {
					union[checkedVariable.getDomain()[i] - min] = true;
					nbVal++;
				}

			}

			if (position != checkPos && checkedVariable.getDomainSize() == 1) {
				final int index = variable
						.index(checkedVariable.getDomain()[checkedVariable
								.getFirst()]);
				if (index >= 0 && variable.isPresent(index)) {
					variable.remove(index, level);
					revised = true;
					setActive(true);
				}
			}
		}

		if (nbVal < getArity()) {
			variable.empty(level);
			setActive(true);
			return true;
		}

		return revised;
	}

//	@Override
//	public int nbTuples(final int variablePosition, final int index) {
//		int nbTuples = 1;
//		for (int i = getArity(); --i >= 0;) {
//			if (i != variablePosition) {
//				nbTuples *= getInvolvedVariables()[i].getDomainSize()-1;
//			}
//		}
//		return nbTuples;
//	}

	public boolean useTupleCache() {
		return false;
	}
	
    public boolean removeTuple(final List<Variable> scope,
            final List<Integer> tuple) {
        return false;
    }
}
