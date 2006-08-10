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

package cspfj.solver;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class WDegOnDom {

	private final Problem problem;

	public WDegOnDom(final Problem prob) {
		super();
		problem = prob;

	}

	public Variable selectVariable() {

		float best = 0;
		Variable bestVariable = null;
		for (Variable v : problem.getVariables()) {
			if (v.isAssigned()) {
				continue;
			}

			

			final float est;

			if (v.getDomainSize() == 1) { // || !v.isSelectable()) {
				est = 0;
			} else {
				float count = 0;
				for (Constraint c : v.getInvolvingConstraints()) {
					count += c.getFreedomDegree() * c.getWeight();

				}

				est = count / v.getDomainSize();
			}
			if (bestVariable == null || est > best) {
				best = est;
				bestVariable = v;

			}

			// System.out.print(est);
			// System.out.print(" ");
		}
		// System.out.println("Selected : " + bestVariable + "(" + best + ")");
		return bestVariable;
	}

}
