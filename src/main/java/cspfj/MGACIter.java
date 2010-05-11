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
import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.CrossHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Lexico;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.problem.NoGoodLearner.LearnMethod;

public final class MGACIter extends AbstractSolver {

	private static final Logger LOGGER = Logger.getLogger(MGACIter.class
			.getName());

	private static final float BT_GROWTH = 1.5f;
	private static final LearnMethod LEARN_METHOD;

	private final Deque<Pair> decisions;

	static {
		final String lm = AbstractSolver.PARAMETERS.get("mgac.addConstraints");
		if (lm == null) {
			LEARN_METHOD = LearnMethod.NONE;
		} else {
			LEARN_METHOD = LearnMethod.valueOf(lm);
		}
	}
	private final Filter filter;

	private final Heuristic heuristic;

	private final NoGoodLearner ngl;

	public MGACIter(final Problem prob) {
		this(prob, new CrossHeuristic(new WDegOnDom(prob), new Lexico(false)));
	}

	public MGACIter(final Problem prob, final Heuristic heuristic) {
		this(prob, heuristic, new AC3(prob));
	}

	public MGACIter(final Problem prob, final Heuristic heuristic,
			final Filter filter) {
		super(prob);
		// filter = new B3C(problem, new BC(problem));
		this.filter = filter;
		this.heuristic = heuristic;

		LOGGER.info(filter.getClass().toString());

		setMaxBacktracks(prob.getMaxBacktracks());

		ngl = new NoGoodLearner(problem, LEARN_METHOD);
		decisions = new LinkedList<Pair>();
	}

	private boolean firstSolutionGiven = false;

	public Map<Variable, Integer> mac(final boolean skipFirstSolution)
			throws MaxBacktracksExceededException, IOException {
		final Problem problem = this.problem;
		boolean skipSolution = skipFirstSolution;
		Variable selectedVariable = null;
		int selectedIndex = -1;
		for (;;) {
			if (selectedVariable != null
					&& !filter.reduceAfter(selectedVariable)) {
				selectedVariable = backtrack();
				if (selectedVariable == null) {
					break;
				}
				continue;
			}

			final Pair pair = heuristic.selectPair(problem);

			if (pair == null) {
				if (skipSolution) {
					selectedVariable = backtrack();
					if (selectedVariable == null) {
						break;
					}
					skipSolution = false;
					continue;
				}
				return solution();
			}
			decisions.push(pair);
			selectedVariable = pair.getVariable();

			assert selectedVariable.getDomainSize() > 0;

			selectedIndex = pair.getIndex();

			assert selectedVariable.isPresent(selectedIndex);

			LOGGER.fine(problem.getCurrentLevel() + " : " + selectedVariable
					+ " <- "
					+ selectedVariable.getDomain().value(selectedIndex) + "("
					+ getNbBacktracks() + "/" + getMaxBacktracks() + ")");

			problem.setCurrentLevelVariable(selectedVariable);
			problem.push();
			selectedVariable.getDomain().setSingle(selectedIndex);
			incrementNbAssignments();

		}
		return null;

	}

	private Variable backtrack() throws MaxBacktracksExceededException {
		Pair decision;
		do {
			if (decisions.isEmpty()) {
				return null;
			}
			decision = decisions.pop();
			// decision.getVariable().unassign();
			problem.pop();
			problem.setCurrentLevelVariable(null);
		} while (decision.getVariable().getDomainSize() <= 1);

		LOGGER
				.finer(problem.getCurrentLevel() + " : "
						+ decision.getVariable() + " /= "
						+ decision.getVariable().getValue(decision.getIndex()));
		decision.getVariable().remove(decision.getIndex());
		checkBacktracks();
		return decision.getVariable();
	}

	@Override
	public Map<Variable, Integer> nextSolution() throws IOException {
		// System.gc();
		int maxBT;
		if (firstSolutionGiven) {
			maxBT = -1;
		} else {
			try {
				if (!preprocess(filter)) {
					firstSolutionGiven = true;
					return null;
				}
			} catch (InterruptedException e) {
				try {
					if (!filter.reduceAll()) {
						return null;
					}
				} catch (InterruptedException e1) {
					throw new IllegalStateException("Unexpected interruption");
				}
			}

			long heuristicCpu = -System.currentTimeMillis();
			heuristic.compute();

			heuristicCpu += System.currentTimeMillis();
			statistic("heuristic-cpu", heuristicCpu / 1000f);

			maxBT = getMaxBacktracks();
		}

		for (;;) {
			setMaxBacktracks(maxBT);

			LOGGER.info("MAC with " + maxBT + " bt");
			long macTime = -System.currentTimeMillis();
			int nbBT = getNbBacktracks();

			try {
				final Map<Variable, Integer> solution = mac(firstSolutionGiven);
				firstSolutionGiven = true;
				return solution;
			} catch (MaxBacktracksExceededException e) {
				final Set<Constraint> modified = ngl.noGoods(decisions);
				problem.reset();
				problem.clearLevelVariables();
				decisions.clear();

				maxBT *= BT_GROWTH;

				try {
					if (!filter.reduceAll()) {
						break;
					}
				} catch (InterruptedException e2) {
					throw new IllegalStateException(
							"Filter was unexpectingly interrupted by " + e2);
				}

			} finally {
				macTime += System.currentTimeMillis();
				increaseStatistic("search-cpu", macTime / 1000f);
				LOGGER.info("Took " + (macTime / 1000f) + "s ("
						+ (1000f * (getNbBacktracks() - nbBT) / macTime)
						+ " bps)");
			}

		}

		return null;

	}

	public void collectStatistics() {
		for (Entry<String, Object> e : filter.getStatistics().entrySet()) {
			statistic(e.getKey(), e.getValue());
		}
	}

	public Filter getFilter() {
		return filter;
	}

	public String getXMLConfig() {
		return super.getXMLConfig() + "\t\t\t<solver>" + this
				+ "</solver>\n\t\t\t<filter>" + filter
				+ "</filter>\n\t\t\t<heuristic>" + heuristic
				+ "</heuristic>\n\t\t\t<prepro>" + getPreprocessor()
				+ "</prepro>\n";

	}

	public String toString() {
		return "maintain generalized arc consistency - iterative";
	}

}
