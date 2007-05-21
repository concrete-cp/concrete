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

	// final List<Integer> nbc = new ArrayList<Integer>(500);
	// final List<Integer> nbrc = new ArrayList<Integer>(500);

	private int nbLM =0;
	
	private final TieManager tieManager;

	public WMC(Problem prob, ResultHandler resultHandler, boolean max) {
		super(prob, resultHandler, max);
		tieManager = new TieManager(getRandom());
		setMaxBacktracks(2000);
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

	private int localMinimum() throws MaxBacktracksExceededException {
		int improvment = 0;

		// if (FINER) {
		logger.finer("Local Minimum");
		// }
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
						final ConflictsManager cm = wcManagers[involvedVariables[pos]
								.getId()];
						cm.updateAfterIncrement(c, pos);
						changed |= cm.getBestImprovment() < 0;

					}
				}
			}
			// incrementNbAssignments();
			checkBacktracks();
			// nbc.add(weightedConflicts());
			// nbrc.add(realConflicts());
			nbLM++;
		} while (!changed);

		// initBestVariable() ;

		return improvment;
	}

	private int bestWalk() throws MaxBacktracksExceededException {

		ConflictsManager bestCM = null;
		final TieManager tieManager = this.tieManager;
		tieManager.clear();

		for (ConflictsManager wcm : wcManagers) {
			if (wcm.getCurrentConflicts() == 0) {
				continue;
			}
			// if (!wcm.isCritic()) {
			// continue;
			// }
			final int bestIndex = wcm.getBestIndex();
			if (bestIndex >= 0
					&& tieManager.newValue(bestIndex, wcm
							.getImprovment(bestIndex))) {
				bestCM = wcm;
				// bestImp = imp;

			}

		}

		final int imp = tieManager.getBestEvaluation();
		// logger.finer("Proposed " + bestIndex + " <- " + bestCM.getVariable()
		// + " ("+imp+")");
		if (imp >= 0) {
			return localMinimum();
		}

		if (FINER) {
			logger.finer(bestCM.getVariable() + " <- " + bestCM.getBestIndex());
		}
		bestCM.getVariable().increaseWeight(1);
		reAssign(bestCM, tieManager.getBestValue());
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
			// nbc.add(nbConflicts);
			// nbrc.add(realConflicts());
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

		// for (int i=0 ; i < nbc.size() ; i++) {
		// System.out.println(i+" "+nbc.get(i)+" "+nbrc.get(i));
		// }

		incrementNbSolutions();
		assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
				+ " conflicts ! (" + weightedConflicts() + " wc)";
		statistics("local-minima", nbLM);
	}

	public String toString() {
		return "weighted min-conflicts";
	}

	// public int getMaxFlips() {
	// return super.getMaxFlips()/10;
	// }
}
