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

public final class MCRW extends AbstractLocalSolver {

	private final static Logger logger = Logger.getLogger("cspfj.WMC");

	private final float randomWalk;

	public MCRW(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);
		randomWalk = .02F;

	}

	private Variable findBest() {
		Variable bestVariable = null;
		final TieManager tieManager = getTieManager();
		tieManager.clear();

		int bestImp = tieManager.getBestEvaluation();

		for (Variable v : problem.getVariables()) {
			final int vId = v.getId();
			final ConflictsManager wcm = wcManagers[vId];
			if (wcm.getCurrentConflicts() <= -bestImp) {
				continue;
			}

			final int imp = wcm.getBestImprovment();
			if (tieManager.newValue(imp)) {
				bestVariable = v;
				bestImp = imp;
			}

		}

		return bestVariable;

	}

	private int bestWalk() throws MaxBacktracksExceededException {
		final Variable bestVariable = findBest();
		final ConflictsManager wcm = wcManagers[bestVariable.getId()];

		final int bestIndex = wcm.getBestIndex();

		if (wcm.getBestIndex() == bestVariable.getFirst()) {
			return 0;
		}
		flipped(bestVariable);
		final int improvment = wcm.getBestImprovment();

		if (FINER) {
			logger.finer(bestVariable + " <- " + bestIndex);
		}
		reAssign(wcm, bestIndex);
		incrementNbAssignments();
		checkBacktracks();
		return improvment;
	}

	private int randomWalk() throws MaxBacktracksExceededException {
		final Variable variable = problem.getVariables()[getRandom().nextInt(
				problem.getNbVariables())];

		int index;

		do {
			index = getRandom().nextInt(variable.getDomain().length);
		} while (variable.getRemovedLevel(index) >= 0);

		if (index == variable.getFirst()) {
			return 0;
		}
		flipped(variable);
		final ConflictsManager wcm = wcManagers[variable.getId()];

		final int improvment = wcm.getImprovment(index);

		reAssign(wcm, index);

		incrementNbAssignments();
		checkBacktracks();
		return improvment;

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

			if (getRandom().nextFloat() < randomWalk) {
				nbConflicts += randomWalk();
			} else {
				nbConflicts += bestWalk();
			}

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
}
