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

import cspfj.problem.Variable;

public final class DTPConstraint extends AbstractPVRConstraint {

	final private int[] duration;

	final private boolean ordered;

	public DTPConstraint(final Variable[] scope, final int duration0,
			final int duration1) {
		this(scope, duration0, duration1, false);
	}

	public DTPConstraint(final Variable[] scope, final int duration0,
			final int duration1, final boolean ordered) {
		super(scope);
		this.duration = new int[] { duration0, duration1 };
		// System.out.println(this);
		this.ordered = ordered;
	}

	@Override
	public boolean check() {
		final int difference = getValue(0) - getValue(1);

		return (-difference >= duration[0] || difference >= duration[1]);
	}

	public String toString() {
		return "C" + getId() + " (" + getVariable(0) + ":" + duration[0] + ", "
				+ getVariable(1) + ":" + duration[1] + ")";
	}

	@Override
	public boolean revise(final int position, final int level) {
		final Variable variable = getVariable(position);
		final Variable otherVariable = getVariable(1 - position);

		final int highBound;
		final int lowBound;

		if (ordered) {

			highBound = otherVariable.getDomain()[otherVariable.getLast()]
					- duration[position];

			lowBound = otherVariable.getDomain()[otherVariable.getFirst()]
					+ duration[1 - position];

		} else {
			int otherMin = otherVariable.getDomain()[otherVariable.getFirst()];
			int otherMax = otherMin;

			for (int i = otherVariable.getFirst(); i >= 0; i = otherVariable
					.getNext(i)) {
				final int value = otherVariable.getDomain()[i];
				if (value < otherMin) {
					otherMin = value;
				}
				if (value > otherMax) {
					otherMax = value;
				}

			}

			highBound = otherMax - duration[position];
			lowBound = otherMin + duration[1 - position];
		}

		boolean filtered = false;

		for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
			final int value = variable.getDomain()[i];
			if (value > highBound && value < lowBound) {
				variable.remove(i, level);
				filtered = true;
			}
		}

		return filtered;
	}

	@Override
	public boolean isSlow() {
		return false;
	}
}
