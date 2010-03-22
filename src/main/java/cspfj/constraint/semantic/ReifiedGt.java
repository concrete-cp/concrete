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

import cspfj.constraint.AbstractAC3Constraint;
import cspfj.problem.Variable;

public final class ReifiedGt extends AbstractAC3Constraint {

	final private boolean strict;

	public ReifiedGt(final Variable result, final Variable v0,
			final Variable v1, final boolean strict) {
		super(result, v0, v1);
		this.strict = strict;
	}

	@Override
	public boolean check() {
		if (strict) {
			return getValue(0) == (getValue(1) > getValue(2) ? 1 : 0);
		}
		return getValue(0) == (getValue(1) >= getValue(2) ? 1 : 0);
	}

	public String toString() {
		if (strict) {
			return getVariable(0) + " = " + getVariable(1) + " > "
					+ getVariable(2);
		}
		return getVariable(0) + " = " + getVariable(1) + " >= "
				+ getVariable(2);
	}
}
