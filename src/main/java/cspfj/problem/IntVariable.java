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

package cspfj.problem;

import cspfj.constraint.Constraint;
import cspfj.constraint.DynamicConstraint;
import cspfj.generator.DomainGenerator;
import cspfj.priorityqueues.Identified;
import cspfj.util.BitVector;
import cspom.variable.CSPOMDomain;
import cspom.variable.CSPOMVariable;

public final class IntVariable extends AbstractVariable implements Cloneable,
		Identified {


	IntDomain domain;

	public IntVariable(final CSPOMVariable cspomVariable) {
		super(cspomVariable);
		domain = DomainGenerator.generate(cspomVariable.getDomain());

	}

	@Override
	public String toString() {
		if (domain == null) {
			return getCspomVariable().getName() + " [?]";
		}
		return getCspomVariable().getName() + " [" + getDomainSize() + "]";
	}

	/**
	 * @return La taille du domaine
	 */
	public int getDomainSize() {
		if (domain == null) {
			return getCspomVariable().getDomain().getValues().size();
		}
		return domain.size();
	}

	public void remove(final int index) {
		assert !assigned : "Trying to remove a value from an assigned variable";
		assert domain.present(index);

		domain.remove(index);
	}

	public IntDomain getDomain() {
		return domain;
	}

	public int getFirst() {
		return domain.first();
	}

	public int getLast() {
		return domain.last();
	}

	public int[] getCurrentIndexes() {
		if (domain == null) {
			return null;
		}
		final int[] indexes = new int[domain.size()];

		for (int i = getFirst(), j = 0; i >= 0; i = getNext(i), j++) {
			indexes[j] = i;
		}

		return indexes;
	}

	public int getNext(final int index) {
		return domain.next(index);
	}

	public int getPrev(final int index) {
		return domain.prev(index);
	}

	public int getLastAbsent() {
		return domain.lastAbsent();
	}

	public int getPrevAbsent(final int index) {
		return domain.prevAbsent(index);
	}

	public BitVector getBitDomain() {
		return ((BitVectorDomain) domain).getBitVector();
	}

	public IntVariable clone() throws CloneNotSupportedException {
		final IntVariable variable = (IntVariable) super.clone();

		variable.domain = domain.clone();

		return variable;
	}

	public boolean isPresent(int index) {
		return domain.present(index);
	}

	public int getValue(int index) {
		return domain.value(index);
	}

	public void makeSingleton(int value1) {
		domain.setSingle(value1);
	}
}
