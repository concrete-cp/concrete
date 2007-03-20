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

import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3_P;
import cspfj.filter.AC3_R;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.filter.SAC.SPACE;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Pair;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class MGAC extends AbstractSolver {

	private final Filter filter;

	private final Heuristic heuristic;

	private static final Logger logger = Logger.getLogger("cspfj.MACSolver");

	private boolean allSolutions = false;

	private final Filter sac;

	// private NoGoodManager noGoodManager = null;

	// private int maxNoGoodSize;

	public MGAC(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, boolean reverse) {
		super(prob, resultHandler);
		filter = reverse ? new AC3_R(problem) : new AC3_P(problem);
		// heuristic = new WDegOnDomBySupports(prob);
		this.heuristic = heuristic;

		sac = new SAC(problem, filter, true);

		logger.info(filter.getClass().toString());

	}

	// public void enableNoGoods(final int maxSize) {
	// // noGoodManager = new NoGoodManager(problem.getNbVariables(), 2);
	// maxNoGoodSize = maxSize;
	// }

	public boolean mac(final int level, final Variable lastModifiedVariable)
			throws MaxBacktracksExceededException {
		return mac(level, lastModifiedVariable, true);
	}

	public boolean mac(final int level, final Variable lastModifiedVariable,
			final boolean positive) throws MaxBacktracksExceededException {
		final Problem problem = this.problem;

		if (problem.getNbFutureVariables() == 0) {
			if (getNbSolutions() < 1) {
				for (Variable v : problem.getVariables()) {
					addSolutionElement(v, v.getFirst());
					// logger.fine(v+" "+v.getFirst());
				}
			}
			incrementNbSolutions();
			return !allSolutions;
		}

		if (positive && level < problem.getMaxArity()
				&& useSpace() == SPACE.BRANCH) {
			if (!sac.reduceAfter(level, lastModifiedVariable)) {
				return false;
			}
		} else {
			if (!filter.reduceAfter(level, lastModifiedVariable)) {
				return false;
			}
		}

		final long pair = heuristic.selectPair(problem);

		final Variable selectedVariable = Pair.variable(pair, problem);

		assert selectedVariable.getDomainSize() > 0;

		final int selectedIndex = Pair.index(pair, problem);

		assert selectedVariable.isPresent(selectedIndex);

		// final int selectedIndex = selectedVariable.getFirstPresentIndex();

		final int domainSizeBefore = selectedVariable.getDomainSize();

		if (logger.isLoggable(Level.FINE)) {
			logger.fine(level + " : " + selectedVariable + " <- "
					+ selectedVariable.getDomain()[selectedIndex] + "("
					+ getNbBacktracks() + "/" + getMaxBacktracks() + ")");
		}

		selectedVariable.assign(selectedIndex, problem);

		problem.setLevelVariables(level, selectedVariable.getId());

		incrementNbAssignments();

		// removeNoGoods(level);

		if (mac(level + 1, domainSizeBefore > 1 ? selectedVariable : null, true)) {
			addSolutionElement(selectedVariable, selectedIndex);
			return true;
		}

		// problem.increaseWeights();
		selectedVariable.unassign(problem);
		problem.restore(level + 1);

		problem.setLevelVariables(level, -1);

		if (selectedVariable.getDomainSize() <= 1) {
			return false;
		}
		// System.out.println(level + " : " + selectedVariable + " /= "
		// + selectedVariable.getDomain()[selectedIndex]);
		selectedVariable.remove(selectedIndex, level);

		checkBacktracks();

		return mac(level, selectedVariable, false);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see cspfj.Solver#run(int)
	 */
	public boolean runSolver() {

		System.gc();
		// enableNoGoods(2);
		chronometer.startChrono();

		final Filter preprocessor;
		switch (useSpace()) {
		case BRANCH:
			preprocessor = new SAC(problem, getFilter(), true);
			break;

		case CLASSIC:
			preprocessor = new SAC(problem, getFilter(), false);
			break;

		default:

			preprocessor = getFilter();
		}

		final float start = chronometer.getCurrentChrono();
		if (!preprocessor.reduceAll(0)) {
			chronometer.validateChrono();
			return false;
		}

		int removed = 0;

		for (Variable v : problem.getVariables()) {
			removed += v.getDomain().length - v.getDomainSize();

		}

		statistics("prepro-removed", removed);
		// statistics("prepro-subs", preprocessor.getNbSub()) ;
		statistics("prepro-nogoods", preprocessor.getNbNoGoods());
		statistics("prepro-cpu", chronometer.getCurrentChrono() - start);
		statistics("prepro-ccks", Constraint.getNbChecks());
		statistics("prepro-nbpresencechecks", Constraint.getNbPresenceChecks());

		statistics("prepro-nbeffectiverevisions", Constraint
				.getNbEffectiveRevisions());
		statistics("prepro-nbuselessrevisions", Constraint
				.getNbUselessRevisions());
		// statistics("prepro-nbskippedrevisions", Constraint
		// .getNbSkippedRevisions());

		if (preprocessor instanceof SAC) {
			statistics("prepro-singletontests", ((SAC) preprocessor)
					.getNbSingletonTests());
		}

		heuristic.compute();

		int maxBT = allSolutions ? -1 : problem.getMaxBacktracks();

		final Filter filter = getFilter();

		//
		// logger.fine("ok!") ;

		// final Random random = new Random(0);
		do {
			for (Variable v : problem.getVariables()) {
				assert v.getDomainSize() > 0;
			}

			logger.info("MAC with " + maxBT + " bt");
			float macTime = -chronometer.getCurrentChrono();
			// System.out.print("run ! ");
			try {
				setMaxBacktracks(maxBT);
				mac(0, null);

				break;
			} catch (MaxBacktracksExceededException e) {
				// On continue...
			} catch (OutOfMemoryError e) {
				chronometer.validateChrono();
				throw e;
			}
			macTime += chronometer.getCurrentChrono();
			logger
					.info("Took " + macTime + "s (" + (maxBT / macTime)
							+ " bps)");
			maxBT *= 1.5;
			addNoGoods();
			problem.restoreAll(1);
			if (!filter.reduceAll(0)) {
				chronometer.validateChrono();
				return false;
			}
		} while (true);

		chronometer.validateChrono();

		return getNbSolutions() > 0;

	}

	public int addNoGoods() {
		return problem.addNoGoods();
	}

	//
	// private void removeNoGoods(final int level) {
	//
	// logger.info("removing noGoods");
	//
	// final Variable scope[] = new Variable[level + 1];
	// final int[] tuple = new int[level + 1];
	//
	// for (int l = 0; l <= level; l++) {
	// scope[l] = problem.getVariable(levelVariables[l]);
	// tuple[l] = problem.getVariable(levelVariables[l])
	// .getFirstPresentIndex();
	// }
	//
	// for (int[] couple : noGoodManager.removable(scope, tuple)) {
	// if (problem.getVariable(couple[0]).isPresent(couple[1])) {
	// problem.getVariable(couple[0]).remove(couple[1], level);
	// logger.info(couple[0] + " /= " + couple[1]);
	// }
	// }
	//
	// } // private void removeNoGoods(final int level) { // final Variable

	// scope[] = new Variable[level + 1];
	// final int[] tuple = new int[level + 1];
	//
	// for (int l = 0; l <= level; l++) {
	// scope[l] = problem.getVariable(levelVariables[l]);
	// tuple[l] = problem.getVariable(levelVariables[l])
	// .getFirstPresentIndex();
	// }
	//
	// final int[] noGood = noGoodManager.createNoGood(scope, tuple);
	//
	// for (Variable v : problem.getVariables()) {
	// if (noGood[v.getId()] >= 0) {
	// continue;
	// }
	//
	// for (int i = v.getFirstPresentIndex(); i < v.getDomain().length; i++) {
	// if (!v.isPresent(i)) {
	// continue;
	// }
	//
	// noGood[v.getId()] = i;
	//
	// if (noGoodManager.isNoGood(noGood)) {
	// v.remove(i, level);
	// }
	// }
	//
	// noGood[v.getId()] = -1;
	// }
	//
	// }

	// public NoGoodManager getNoGoodManager() {
	// return noGoodManager;
	// }

	public Filter getFilter() {
		return filter;
	}

	public void setAllSolutions(final boolean allSolutions) {
		this.allSolutions = allSolutions;
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer(180);

		sb.append("\t\t\t<solver>").append(this).append(
				"</solver>\n\t\t\t<filter>").append(filter).append(
				"</filter>\n\t\t\t<heuristic>").append(heuristic).append(
				"</heuristic>\n\t\t\t<space>").append(useSpace()).append(
				"</space>\n\t\t\t<allSolutions>").append(allSolutions).append(
				"</allSolutions>\n");

		return sb.toString();
	}

	public String toString() {
		return "maintain generalized arc consistency";
	}
	// public int getMaxNoGoodSize() {
	// return maxNoGoodSize;
	// }

}
