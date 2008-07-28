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

public final class DiffConstraint extends AbstractPVRConstraint {

	final private int constant;

	final private boolean ordered;

	public DiffConstraint(final Variable[] scope, final int constant) {
		this(scope, constant, false);
	}

	public DiffConstraint(final Variable[] scope, final int constant,
			final boolean ordered) {
		super(scope);
		this.constant = constant;
		this.ordered = ordered;
	}

	@Override
	public boolean check() {

		return getValue(0) - getValue(1) <= constant;
	}

	public boolean revise(final int position, final int level) {
		final Variable v0 = getVariable(0);
		final Variable v1 = getVariable(1);

		boolean deleted = false;

		if (position == 0) {
			int v1max = v1.getDomain()[v1.getLast()];
			if (!ordered) {
				for (int i = v1.getLast(); i != -1; i = v1.getPrev(i)) {
					if (v1.getDomain()[i] > v1max) {
						v1max = v1.getDomain()[i];
					}
				}
			}

			for (int i = v0.getFirst(); i != -1; i = v0.getNext(i)) {
				if (v0.getDomain()[i] - constant > v1max) {
					v0.remove(i, level);
					deleted = true;
				}
			}
		} else {
			int v0min= v0.getDomain()[v0.getFirst()];
			if (!ordered) {
				for (int i = v0.getFirst(); i != -1; i = v0.getNext(i)) {
					if (v0.getDomain()[i] < v0min) {
						v0min = v1.getDomain()[i];
					}
				}
			}

			for (int i = v1.getFirst(); i != -1; i = v1.getNext(i)) {
				if (v1.getDomain()[i] + constant < v0min) {
					v1.remove(i, level);
					deleted = true;
				}
			}
		}
		return deleted;
	}

	@Override
	public boolean isSlow() {
		return false;
	}

	// public String toString() {
	// return "C" + getId() + " (" + getInvolvedVariables()[0] + ":"
	// + duration0 + ", " + getInvolvedVariables()[1] + ":"
	// + duration1 + ")";
	// }

}
