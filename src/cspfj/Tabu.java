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

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class Tabu extends AbstractLocalSolver {

	private final static Logger logger = Logger.getLogger("cspfj.Tabu");

	private final TabuManager tabuManager;

	private final TieManager tieManager;

	public Tabu(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);
		tabuManager = new TabuManager(prob, 25);
		tieManager = new TieManager(getRandom());
	}

	private int bestWalk() {
		Variable bestVariable = null;
		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		int bestImp = tieManager.getBestEvaluation();

		for (Variable v : problem.getVariables()) {
			final int vId = v.getId();
			final WCManager wcm = wcManagers[vId];
			if (wcm.getCurrentConflicts() <= -bestImp) {
				continue;
			}

			final int bestIndex = wcm.getBestIndex(tabuManager);
			if (bestIndex >= 0) {
				final int imp = wcm.getImprovment(bestIndex);

				if (tieManager.newValue(bestIndex, imp)) {
					bestVariable = v;
					bestImp = imp;
				}
			}

		}

		flipped(bestVariable);

		final int bestVariableId = bestVariable.getId();

		final WCManager wcm = wcManagers[bestVariableId];

		final int bestIndex = tieManager.getBestValue();

		tabuManager.push(bestVariableId, bestIndex);

		if (FINER) {
			logger.finer(bestVariable + " <- " + bestIndex);
		}
		reAssign(wcm, bestIndex);
		incrementNbAssignments();
		return bestImp;
	}

	public void minConflicts() throws MaxBacktracksExceededException,
			IOException {

		final Problem problem = this.problem;
		// final float randomWalk = this.rWProba;

		init();
		tabuManager.clean();
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

			checkBacktracks();

		}

		for (Variable v : problem.getVariables()) {
			addSolutionElement(v, v.getFirst());
		}

		incrementNbSolutions();
		assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
				+ " conflicts ! (" + weightedConflicts() + " wc)";

	}

}