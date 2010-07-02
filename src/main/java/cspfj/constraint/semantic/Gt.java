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
import cspfj.problem.Domain;
import cspfj.problem.Variable;

public final class Gt extends AbstractConstraint {

	private final boolean strict;
	private final int constant;

	public Gt(final Variable v0, final Variable v1, final boolean strict) {
		this(v0, 0, v1, strict);
	}

	public Gt(final Variable v0, final int constant, final Variable v1,
			final boolean strict) {
		super(v0, v1);
		this.strict = strict;
		this.constant = constant;
	}

	@Override
	public boolean check() {
		if (strict) {
			return getValue(0) + constant > getValue(1);
		}
		return getValue(0) + constant >= getValue(1);
	}

	private int min(final int position) {
		final Domain dom = getVariable(position).getDomain();
		return dom.value(dom.first());
	}

	private int max(final int position) {
		final Domain dom = getVariable(position).getDomain();
		return dom.value(dom.last());
	}

	private boolean removeGt(final int value, final int position) {
		final Domain dom = getVariable(position).getDomain();
		final int lb = dom.lowest(value);
		if (lb >= 0) {
			if (strict || dom.value(lb) != value) {
				return dom.removeFrom(lb) > 0;
			}
			return dom.removeFrom(lb + 1) > 0;
		}
		return false;
	}

	private boolean removeLt(final int value, final int position) {
		final Domain dom = getVariable(position).getDomain();
		final int ub = dom.greatest(value);
		if (ub >= 0) {
			if (strict || dom.value(ub) != value) {
				return dom.removeTo(ub) > 0;
			}
			return dom.removeTo(ub - 1) > 0;
		}
		return false;

	}

	@Override
	public boolean revise(final RevisionHandler revisator, final int reviseCount) {
		assert getVariable(0).getDomainSize() > 0
				&& getVariable(1).getDomainSize() > 0;

		if (removeLt(min(1) - constant, 0)) {
			if (getVariable(0).getDomainSize() == 0) {
				return false;
			}
			revisator.revised(this, getVariable(0));
		}
		if (removeGt(max(0) + constant, 1)) {
			if (getVariable(1).getDomainSize() == 0) {
				return false;
			}
			revisator.revised(this, getVariable(1));
		}
		final int max1 = max(1);
		final int min0 = min(0) + constant;
		if (max1 < min0 || !strict && min0 == max1) {
			entail();
			return true;
		}
		// System.out.println("Could not entail " + this);
		return true;
	}

	@Override
	public boolean isConsistent(final int reviseCount) {
		final int max0 = max(0) + constant;
		final int min1 = min(1);
		return max0 > min1 || !strict && max0 == min1;
	}

	public String toString() {
		final StringBuilder stb = new StringBuilder();
		stb.append(getVariable(0));

		if (constant > 0) {
			stb.append(" + ").append(constant);
		} else if (constant < 0) {
			stb.append(" - ").append(-constant);
		}

		if (strict) {
			stb.append(" > ");
		} else {
			stb.append(" >= ");
		}

		return stb.append(getVariable(1)).toString();

	}

	@Override
	public float getEvaluation() {
		return Math.max(getVariable(0).getDomainSize(), getVariable(1)
				.getDomainSize());
	}
}
