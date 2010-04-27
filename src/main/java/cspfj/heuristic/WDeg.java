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

package cspfj.heuristic;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class WDeg extends AbstractVariableHeuristic {

	public WDeg(final Problem problem) {
		super(problem);
		for (Constraint c : problem.getConstraints()) {
			c.setWeight(1);
		}
	}

	public double getScore(final Variable variable) {
		return wDeg(variable);
	}

	public static double wDeg(final Variable variable) {
		double count = 0;

		for (Constraint c : variable.getInvolvingConstraints()) {
			count += (nbUnboundVariables(c) - 1) * c.getWeight();
		}
		return count;
	}

	public static int nbUnboundVariables(final Constraint constraint) {
		int count = 0;
		for (Variable v : constraint.getScope()) {
			if (v.getDomainSize() > 1) {
				count++;
			}
		}
		return count;
	}

	public String toString() {
		return "max-wdeg";
	}

}
