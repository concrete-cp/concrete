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
import java.lang.reflect.InvocationTargetException;
import java.security.InvalidParameterException;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.CrossHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Lexico;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class MGACIter extends AbstractSolver {

	private final Filter filter;

	private final Heuristic heuristic;

	private static final Logger logger = Logger.getLogger("cspfj.MACSolver");

	private boolean allSolutions = false;

	private int level;

	public MGACIter(Problem prob, ResultHandler resultHandler) {
		this(prob, resultHandler, new CrossHeuristic(new WDegOnDom(prob),
				new Lexico(prob, false)));
	}

	public MGACIter(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic) {
		this(prob, resultHandler, heuristic, new AC3(prob, heuristic
				.getVariableHeuristic()));
	}

	public MGACIter(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, Filter filter) {
		super(prob, resultHandler);
		// filter = new B3C(problem, new BC(problem));
		this.filter = filter;
		this.heuristic = heuristic;

		logger.info(filter.getClass().toString());

		setMaxBacktracks(prob.getMaxBacktracks());
	}

	public boolean mac() throws MaxBacktracksExceededException, IOException {
		final Problem problem = this.problem;

		level = 0;

		Variable selectedVariable = null;
		int selectedIndex = -1;
		do {
			if (problem.getNbFutureVariables() == 0) {
				if (getNbSolutions() < 1) {
					for (Variable v : problem.getVariables()) {
						addSolutionElement(v, v.getFirst());
					}
				}

				solution();
				if (allSolutions) {
					selectedVariable = backtrack();
					if (level < 0) {
						break;
					}
				} else {
					break;
				}
			}

			if (selectedVariable != null
					&& !filter.reduceAfter(level, selectedVariable)) {
				if (level == 0) {
					break;
				}
				selectedVariable = backtrack();
				if (level < 0) {
					break;
				}
				continue;
			}

			final long pair = heuristic.selectPair(problem);

			selectedVariable = Pair.variable(pair, problem);

			assert selectedVariable.getDomainSize() > 0;

			selectedIndex = Pair.index(pair, problem);

			assert selectedVariable.isPresent(selectedIndex);

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(level + " : " + selectedVariable + " <- "
						+ selectedVariable.getDomain()[selectedIndex] + "("
						+ getNbBacktracks() + "/" + getMaxBacktracks() + ")");
			}

			if (filter.ensureAC()) {
				selectedVariable.assign(selectedIndex, problem);
			} else if (!selectedVariable.assignNotAC(selectedIndex, problem)) {
				selectedVariable.unassign(problem);
				selectedVariable.remove(selectedIndex, level);
				continue;
			}

			problem.setLevelVariables(level, selectedVariable);

			incrementNbAssignments();
			level++;

		} while (true);
		return getNbSolutions() > 0;

	}

	public Variable backtrack() throws MaxBacktracksExceededException {
		Variable selectedVariable;

		do {
			if (level < problem.getNbVariables()) {
				problem.setLevelVariables(level, null);
			}
			selectedVariable = problem.getLevelVariable(--level);
			final int index = selectedVariable.getFirst();
			selectedVariable.unassign(problem);
			problem.restore(level + 1);
			selectedVariable.remove(index, level);

		} while (selectedVariable.getDomainSize() < 1 && level > 0);
		checkBacktracks();
		return selectedVariable;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see cspfj.Solver#run(int)
	 */
	public boolean runSolver() throws IOException {

		System.gc();
		chronometer.startChrono();

		final Filter filter = getFilter();

		try {
			if (!preprocess(filter)) {
				chronometer.validateChrono();
				return false;
			}
		} catch (InstantiationException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (IllegalAccessException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (InvocationTargetException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (NoSuchMethodException e1) {
			throw new InvalidParameterException(e1.toString());
		} catch (InterruptedException e) {
			try {
				if (!filter.reduceAll(0)) {
					chronometer.validateChrono();
					return false;
				}
			} catch (InterruptedException e1) {
				throw new IllegalArgumentException("Unexpected interruption");
			}
		}

		// statistics("prepro-nbskippedrevisions", Constraint
		// .getNbSkippedRevisions());

		// if (true) {
		// return true ;
		// }

		final float start = chronometer.getCurrentChrono();
		heuristic.compute();

		// for (Variable v:problem.getVariables()) {
		// for (int i = v.getFirst() ; i >=0 ; i=v.getNext(i)) {
		// System.out.print(i+"-");
		// }
		// System.out.println();
		// }

		final float heuristicCpu = chronometer.getCurrentChrono();
		statistics.put("heuristic-cpu", heuristicCpu - start);

		int maxBT = allSolutions ? -1 : getMaxBacktracks();

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
				mac();

				break;
			} catch (MaxBacktracksExceededException e) {
				// On continue...
			} catch (OutOfMemoryError e) {
				chronometer.validateChrono();
				throw e;
			} catch (IOException e) {
				chronometer.validateChrono();
				throw e;
			}
			macTime += chronometer.getCurrentChrono();
			logger
					.info("Took " + macTime + "s (" + (maxBT / macTime)
							+ " bps)");

			// logger.info(constraintRepartition());
			maxBT *= 1.5;

			addNoGoods();
			problem.restoreAll(1);
			try {
				if (!filter.reduceAll(0)) {
					chronometer.validateChrono();
					return false;
				}
			} catch (InterruptedException e) {
				throw new IllegalArgumentException(
						"Filter was unexpectingly interrupted !");
			}
		} while (true);
		chronometer.validateChrono();
		statistics.put("search-cpu", chronometer.getCurrentChrono()
				- heuristicCpu);

		statistics.putAll(filter.getStatistics());

		return getNbSolutions() > 0;

	}

	// private String constraintRepartition() {
	// final WeightHeuristic wvh = (WeightHeuristic) heuristic;
	// final SortedSet<Constraint> sortedConstraint = new TreeSet<Constraint>(
	// new Weight(false, wvh));
	// sortedConstraint.addAll(Arrays.asList(problem.getConstraints()));
	//
	// final StringBuilder stb = new StringBuilder();
	// final NumberFormat format = NumberFormat.getInstance();
	// format.setMaximumFractionDigits(2);
	// double total = 0;
	// for (Constraint c : sortedConstraint) {
	// stb.append(c.getName() + "(" + format.format(wvh.getWeight(c)) + ") ");
	// total += wvh.getWeight(c);
	// }
	// stb.append(" - Total = " + total);
	// return stb.toString();
	// }

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
				"</heuristic>\n\t\t\t<prepro>").append(getPreprocessor())
				.append("</prepro>\n\t\t\t<allSolutions>").append(allSolutions)
				.append("</allSolutions>\n");

		return sb.toString();
	}

	public String toString() {
		return "maintain generalized arc consistency - iterative";
	}

}