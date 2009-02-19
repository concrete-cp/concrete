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

package cspfj.constraint.semantic;

import java.util.BitSet;

import cspfj.constraint.AbstractPVRConstraint;
import cspfj.problem.Variable;

public final class AllDifferentConstraint extends AbstractPVRConstraint {

	private final BitSet union;

	private int offset;

	// private final static Logger logger = Logger
	// .getLogger("cspfj.constraint.AllDifferentConstraint");

	public AllDifferentConstraint(final Variable[] scope, final String name) {
		super(scope, name);

		offset = Integer.MAX_VALUE;
		for (Variable v : scope) {
			for (int i : v.getDomain().allValues()) {
				offset = Math.min(i, offset);
			}
		}
		union = new BitSet();
	}

	@Override
	public boolean check() {
		final BitSet union = this.union;
		union.clear();
		final int offset = this.offset;

		final int[] tuple = this.tuple;
		for (int i = getArity(); --i >= 0;) {
			final int value = getVariable(i).getDomain().value(tuple[i]);
			if (union.get(value - offset)) {
				return false;
			}
			union.set(value - offset);
		}
		return true;
	}

	@Override
	public boolean revise(final int position) {
		final Variable variable = getVariable(position);
		assert !variable.isAssigned();

		final BitSet union = this.union;
		final int min = this.offset;

		union.clear();
		boolean revised = false;

		for (int checkPos = getArity(); --checkPos >= 0;) {
			final Variable checkedVariable = getVariable(checkPos);

			for (int i = checkedVariable.getFirst(); i >= 0; i = checkedVariable
					.getNext(i)) {

				union.set(checkedVariable.getDomain().value(i) - min);

			}

			if (position != checkPos && checkedVariable.getDomainSize() == 1) {
				final int index = variable.getDomain().index(
						checkedVariable.getDomain().value(
								checkedVariable.getFirst()));
				if (index >= 0 && variable.isPresent(index)) {
					variable.remove(index);
					revised = true;
					setActive(true);
				}
			}
		}

		if (union.cardinality() < getArity()) {
			setActive(true);
			for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
				variable.remove(i);
			}
			return true;
		}

		return revised;
	}

}
