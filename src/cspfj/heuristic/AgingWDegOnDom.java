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

import java.util.logging.Logger;

import cspfj.constraint.AbstractConstraint;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class AgingWDegOnDom extends AbstractVariableHeuristic implements
		WeightHeuristic {

	private final double[] weights;

	private final double factor;

	private int nbUpdates = 0;

	private final static int AGING_TIME = 100;

	private static final Logger logger = Logger.getLogger(AgingWDegOnDom.class
			.toString());

	public AgingWDegOnDom(Problem problem) {
		super(problem);

		weights = new double[problem.getMaxCId() + 1];
		factor = Math.pow(1 - (1d / problem.getNbConstraints()), AGING_TIME);
		logger.info(Double.toString(factor));
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
		if (nbUpdates++ % AGING_TIME == 0) {
			for (Constraint c : problem.getConstraints()) {
				weights[c.getId()] *= factor;
			}
		}

	}

	@Override
	public double getWeight(final AbstractConstraint constraint) {
		return weights[constraint.getId()];
	}

	public String toString() {
		return "max-wdeg/dom with aging weights";
	}
}
