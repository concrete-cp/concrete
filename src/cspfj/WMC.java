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

package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class WMC extends AbstractLocalSolver {

	private final static Logger logger = Logger.getLogger("cspfj.WMC");

	public WMC(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);

	}

	// private Variable findBest() {
	// Variable bestVariable = null;
	// final TieManager tieManager = getTieManager();
	// tieManager.clear();
	//
	// int bestImp = tieManager.getBestEvaluation();
	//
	// for (Variable v : problem.getVariables()) {
	// final int vId = v.getId();
	// final ConflictsManager wcm = wcManagers[vId];
	// if (wcm.getCurrentConflicts() <= -bestImp) {
	// continue;
	// }
	//
	// final int imp = wcm.getBestImprovment();
	// if (tieManager.newValue(imp)) {
	// bestVariable = v;
	// bestImp = imp;
	// }
	//
	// }
	//
	// return bestVariable;
	//
	// }

	private int localMinimum() {
		int improvment = 0;

		if (FINER) {
			logger.finer("Local Minimum");
		}
		final ConflictsManager[] wcManagers = this.wcManagers;
		boolean changed = false;
		do {
			for (Constraint c : problem.getConstraints()) {
				if (!c.checkFirst()) {
					improvment++;
					c.increaseWeight();
					final Variable[] involvedVariables = c
							.getInvolvedVariables();
					for (int pos = involvedVariables.length; --pos >= 0;) {
						changed |= wcManagers[involvedVariables[pos].getId()]
								.updateAfterIncrement(c, pos);

					}
				}
			}
		} while (!changed);

		initBestVariable() ;
		
		return improvment;
	}

	private int bestWalk() throws MaxBacktracksExceededException {

		final int imp = getBestImp();
		if (imp >= 0) {
			return localMinimum();
		}

		final Variable bestVariable = getBestVariable();

		final ConflictsManager wcm = wcManagers[bestVariable.getId()];

		// final int imp = wcm.getBestImprovment();

		if (FINER) {
			logger.finer(bestVariable + " <- " + wcm.getBestIndex());
		}
		bestVariable.increaseWeight(1);
		reAssign(wcm, wcm.getBestIndex());
		incrementNbAssignments();
		checkBacktracks();
		return imp;
	}

	public void minConflicts() throws MaxBacktracksExceededException,
			IOException {

		final Problem problem = this.problem;
		// final float randomWalk = this.rWProba;

		init();

		int nbConflicts = weightedConflicts();

		logger.fine("Searching...");

		int bestEver = nbConflicts;

		while (nbConflicts > 0) {
			if (nbConflicts < bestEver) {
				bestEver = nbConflicts;
				solution(nbConflicts);
			}

			if (FINER) {
				logger.finer(nbConflicts + " conflicts " + "(real = "
						+ realConflicts() + ", " + getNbBacktracks() + "/"
						+ getMaxBacktracks() + ")");
			}

			assert realConflicts() <= nbConflicts;

			// if (random.nextFloat() < randomWalk) {
			// nbConflicts += randomWalk();
			// } else {
			nbConflicts += bestWalk();
			// }

			assert nbConflicts == weightedConflicts() : nbConflicts + "/="
					+ weightedConflicts() + " (real = " + realConflicts() + ")";

		}

		for (Variable v : problem.getVariables()) {
			addSolutionElement(v, v.getFirst());
		}

		incrementNbSolutions();
		assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
				+ " conflicts ! (" + weightedConflicts() + " wc)";

	}

	public String toString() {
		return "weighted min-conflicts";
	}
	
	public int getMaxFlips() {
		return super.getMaxFlips()/10;
	}
}
