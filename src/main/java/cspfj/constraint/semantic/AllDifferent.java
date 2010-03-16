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

import cspfj.constraint.AbstractConstraint;
import cspfj.filter.RevisionHandler;
import cspfj.problem.Variable;
import cspfj.util.BitVector;

public final class AllDifferent extends AbstractConstraint {

	private final BitVector union;

	private final BitVector queue;

	private int offset;

	// private final static Logger logger = Logger
	// .getLogger("cspfj.constraint.AllDifferentConstraint");

	public AllDifferent(final String name, final Variable... scope) {
		super(name, scope);

		offset = Integer.MAX_VALUE;
		int max = Integer.MIN_VALUE;
		for (Variable v : scope) {
			for (int i : v.getDomain().allValues()) {
				offset = Math.min(i, offset);
				max = Math.max(i, max);
			}
		}
		union = BitVector.factory(max - offset + 1, false);
		queue = BitVector.factory(getArity(), false);
	}

	@Override
	public boolean check() {
		final BitVector singletons = this.union;
		singletons.fill(false);
		final int offset = this.offset;

		final int[] tuple = this.tuple;
		for (int i = getArity(); --i >= 0;) {
			final int value = getVariable(i).getDomain().value(tuple[i]);
			if (singletons.get(value - offset)) {
				return false;
			}
			singletons.set(value - offset);
		}
		return true;
	}

	@Override
	public boolean revise(RevisionHandler revisator, int reviseCount) {
		final BitVector union = this.union;
		final int min = this.offset;
		final BitVector queue = this.queue;

		queue.fill(false);
		for (int pos = getArity(); --pos >= 0;) {
			if (getRemovals(pos) >= reviseCount
					&& getVariable(pos).getDomainSize() == 1) {
				queue.set(pos);
			}
		}

		while (!queue.isEmpty()) {
			final int checkPos = queue.nextSetBit(0);
			queue.clear(checkPos);

			final Variable checkedVariable = getVariable(checkPos);
			final int value = checkedVariable.getDomain().value(
					checkedVariable.getFirst());

			for (int remPos = getArity(); --remPos >= 0;) {
				if (remPos == checkPos) {
					continue;
				}
				final Variable remVariable = getVariable(remPos);
				if (remVariable.isAssigned()) {
					continue;
				}
				final int index = remVariable.getDomain().index(value);
				if (index >= 0 && remVariable.isPresent(index)) {
					remVariable.remove(index);
					if (remVariable.getDomainSize() < 1) {
						return false;
					} else if (remVariable.getDomainSize() == 1) {
						queue.set(remPos);
					}
					revisator.revised(this, remVariable);
				}

			}

		}

		union.fill(false);

		for (Variable v : getScope()) {

			for (int i = v.getFirst(); i >= 0; i = v.getNext(i)) {

				union.set(v.getDomain().value(i) - min);

			}
		}

		if (union.cardinality() < getArity()) {
			setActive(true);
			return false;
		}

		return true;
	}
}
