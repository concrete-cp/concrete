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
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.DiscHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Inverse;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class MGAC extends AbstractSolver {

	private final Filter filter;

	private final Heuristic heuristic;

	private static final Logger logger = Logger.getLogger("cspfj.MACSolver");

	private boolean allSolutions = false;

	public MGAC(Problem prob) {
		this(prob, new ResultHandler());
	}

	public MGAC(Problem prob, ResultHandler resultHandler) {
		this(prob, resultHandler, new DiscHeuristic(new WDegOnDom(),
				new Inverse(prob, false)));
	}

	public MGAC(Problem prob, ResultHandler resultHandler, Heuristic heuristic) {
		super(prob, resultHandler);
		//filter = new B3C(problem, new BC(problem));
		filter = new AC3(problem);
		this.heuristic = heuristic;

		logger.info(filter.getClass().toString());

		setMaxBacktracks(prob.getMaxBacktracks());

	}

	public boolean mac(final int level, final Variable modifiedVariable)
			throws MaxBacktracksExceededException {
		return mac(level, modifiedVariable, true);
	}

	public boolean mac(final int level, final Variable modifiedVariable,
			final boolean positive) throws MaxBacktracksExceededException {
		final Problem problem = this.problem;

		if (problem.getNbFutureVariables() == 0) {
			if (getNbSolutions() < 1) {
				for (Variable v : problem.getVariables()) {
					addSolutionElement(v, v.getFirst());
				}
			}
			incrementNbSolutions();
			return !allSolutions;
		}

		if (!filter.reduceAfter(level, modifiedVariable)) {
			return false;
		}

		final long pair = heuristic.selectPair(problem);

		final Variable selectedVariable = Pair.variable(pair, problem);

		assert selectedVariable.getDomainSize() > 0;

		final int selectedIndex = Pair.index(pair, problem);

		assert selectedVariable.isPresent(selectedIndex);

		final int domainSizeBefore = selectedVariable.getDomainSize();

		if (logger.isLoggable(Level.FINE)) {
			logger.fine(level + " : " + selectedVariable + " <- "
					+ selectedVariable.getDomain()[selectedIndex] + "("
					+ getNbBacktracks() + "/" + getMaxBacktracks() + ")");
		}

		selectedVariable.assign(selectedIndex, problem);

		problem.setLevelVariables(level, selectedVariable);

		incrementNbAssignments();

		if (mac(level + 1, domainSizeBefore > 1 ? selectedVariable : null, true)) {
			addSolutionElement(selectedVariable, selectedIndex);
			return true;
		}

		selectedVariable.unassign(problem);
		problem.restore(level + 1);

		problem.setLevelVariables(level, null);

		if (selectedVariable.getDomainSize() <= 1) {
			return false;
		}
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
		chronometer.startChrono();

		final float start = chronometer.getCurrentChrono();
		if (!preprocess(getFilter())) {
			chronometer.validateChrono();
			return false;
		}

		int removed = 0;

		for (Variable v : problem.getVariables()) {
			removed += v.getDomain().length - v.getDomainSize();

		}

		statistics("prepro-removed", removed);
		// statistics("prepro-subs", preprocessor.getNbSub()) ;

		final float preproCpu = chronometer.getCurrentChrono();
		statistics("prepro-cpu", preproCpu - start);
		statistics("prepro-ccks", Constraint.getNbChecks());
		statistics("prepro-nbpresencechecks", Constraint.getNbPresenceChecks());

		statistics("prepro-nbeffectiverevisions", Constraint
				.getNbEffectiveRevisions());
		statistics("prepro-nbuselessrevisions", Constraint
				.getNbUselessRevisions());
		// statistics("prepro-nbskippedrevisions", Constraint
		// .getNbSkippedRevisions());

		// if (true) {
		// return true ;
		// }

		heuristic.compute();

		final float heuristicCpu = chronometer.getCurrentChrono();
		statistics("heuristic-cpu", heuristicCpu - start);

		int maxBT = allSolutions ? -1 : getMaxBacktracks();

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
		statistics("search-cpu", chronometer.getCurrentChrono() - heuristicCpu);
		chronometer.validateChrono();

		return getNbSolutions() > 0;

	}

	public int addNoGoods() {
		return problem.addNoGoods();
	}

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

}
