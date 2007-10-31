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

package cspfj.filter;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class FC implements Filter {

	public FC(final Problem problem) {
		super();
	}

	public boolean reduceAll(final int level) {
		return true;
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		final Constraint[] constraints = variable.getInvolvingConstraints();

		for (int c = constraints.length; --c >= 0;) {

			final Constraint constraint = constraints[c];

			for (int i = constraint.getArity(); --i >= 0;) {
				final Variable y = constraint.getInvolvedVariables()[i];

				if (!y.isAssigned() && constraint.revise(i, level)) {
					if (y.getDomainSize() <= 0) {
						constraint.increaseWeight();
						return false;
					}

				}
			}

		}
		return true;

	}

	public int getNbNoGoods() {
		return 0;
	}

	public String toString() {
		return "FC";
	}

}
