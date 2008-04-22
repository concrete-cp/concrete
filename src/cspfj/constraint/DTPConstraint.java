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

public final class DTPConstraint extends Constraint {

	final private int duration0;

	final private int duration1;

	public DTPConstraint(final Variable[] scope, final int duration0,
			final int duration1) {
		super(scope, false);
		this.duration0 = duration0;
		this.duration1 = duration1;
	}

	@Override
	public boolean check() {
		final int difference = getValue(0) - getValue(1);

		return (difference <= duration0 || -difference <= duration1);
	}

	public String toString() {
		return "C" + getId() + " (" + getInvolvedVariables()[0] + ":"
				+ duration0 + ", " + getInvolvedVariables()[1] + ":"
				+ duration1 + ")";
	}

}
