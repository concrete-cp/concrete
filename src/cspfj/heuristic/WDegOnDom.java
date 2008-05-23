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

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class WDegOnDom extends AbstractVariableHeuristic implements
		WeightHeuristic {

	private final double[] weights;

	public WDegOnDom(Problem problem) {
		super(problem);

		weights = new double[problem.getMaxCId() + 1];
	}

	public double getScore(final Variable variable) {
		return wDeg(variable) / variable.getDomainSize();
	}

	private double wDeg(final Variable variable) {
		double count = 0;

		for (Constraint c : variable.getInvolvingConstraints()) {
			if (c.isBound()) {
				count += weights[c.getId()];
			}
		}
		return count;
	}

	@Override
	public void treatConflictConstraint(final Constraint constraint) {
		weights[constraint.getId()]++;
	}

	@Override
	public double getWeight(final Constraint constraint) {
		return weights[constraint.getId()];
	}

	public String toString() {
		return "max-wdeg/dom";
	}
}
