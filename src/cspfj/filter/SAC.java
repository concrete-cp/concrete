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
package cspfj.filter;

import java.util.Arrays;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.exception.OutOfTimeException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

/**
 * @author Julien VION
 * 
 */
public final class SAC implements Filter {

	private final Filter filter;

	// private final int maxNoGoodSize;

	private final static Logger logger = Logger.getLogger("cspfj.filter.SAC");

	private final Chronometer chronometer;

	private final Problem problem;

	private final boolean branch;

	// private final boolean[] inQueue;

	private final boolean[][] queue;

	private final boolean[] vQueue;

	// private final Map<Integer, SortedSet<Integer>> indexQueues;

	private int nbNoGoods = 0;

	// private int nbSub = 0;

	private int nbSingletonTests = 0;

	private Variable selectedVariable;

	private int selectedIndex;

	//private final VariableHeuristic heuristic;

	public SAC(Problem problem, Chronometer chronometer, Filter filter,
			boolean branch) {
		super();
		this.filter = filter;
		// this.maxNoGoodSize = maxNoGoodSize;
		this.chronometer = chronometer;
		this.problem = problem;
		this.branch = branch;

		queue = new boolean[problem.getMaxVId() + 1][];
		vQueue = new boolean[problem.getMaxVId() + 1];
		for (Variable v : problem.getVariables()) {
			queue[v.getId()] = new boolean[v.getDomain().length];
		}

//		heuristic = new WDegOnDom(problem);

		// indexQueues = new HashMap<Integer, SortedSet<Integer>>();
		// for (Variable variable : problem.getVariables()) {
		// final SortedSet<Integer> indexQueue = new TreeSet<Integer>();
		// indexQueues.put(variable.getId(), indexQueue);
		// }

		// queue = new Maximier<Variable>(new
		// Variable[problem.getNbVariables()]);
		// inQueue = new boolean[problem.getNbVariables()];
	}

	private void fillQueue() {
		final boolean[][] queue = this.queue;
		Arrays.fill(vQueue, true);
		for (Variable variable : problem.getVariables()) {
			Arrays.fill(queue[variable.getId()], true);
		}
	}

	private boolean pull() {
		if (selectedVariable != null
				&& queue[selectedVariable.getId()][selectedIndex]) {
			queue[selectedVariable.getId()][selectedIndex] = false;
			return true;
		}

		final boolean[] vQueue = this.vQueue;

		for (Variable v : problem.getVariables()) {
			if (vQueue[v.getId()]) {
				final int index = getRemainingIndex(v);
				if (index >= 0) {
					selectedVariable = v;
					selectedIndex = index;
					queue[v.getId()][index] = false;
					return true;
				}
				vQueue[v.getId()] = false;
			}
		}
		return false;
	}

	private boolean reduce(final int level) throws OutOfTimeException {
		final Problem problem = this.problem;

		logger.info("SAC");

		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}

		fillQueue();

		while (pull()) {
			final Variable variable = selectedVariable;
			final int index = selectedIndex;

			boolean changedGraph = false;

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(level + " : " + variable + " <- "
						+ variable.getDomain()[index]);
			}

			variable.assign(index, problem);
			problem.setLevelVariables(level, variable.getId());
			nbSingletonTests++;
			if (filter.reduceAfter(level + 1, variable)) {

				if (branch && buildBranch(level + 1, null)) {
					reduceToSolution(level);
					return true;
				}

				final int nbNg = problem.addNoGoods();
				nbNoGoods += nbNg;

				problem.restoreAll(level + 1);
			} else {
				problem.restoreAll(level + 1);

				variable.remove(index, level);
				changedGraph = true;

				if (variable.getDomainSize() <= 0) {
					return false;
				}
			}

			for (int l = level; l < problem.getNbVariables(); l++) {
				problem.setLevelVariables(l, -1);
			}
			// setValueHeuristic(variable, index);

			if (changedGraph && !filter.reduceAfter(level, variable)) {
				return false;
			}

		}

		return true;

	}

	// private boolean substitute(final Variable variable,
	// final Map<Integer, Map<Variable, int[]>> substitutes,
	// final int level) throws OutOfTimeException {
	// boolean changedGraph = false;
	// for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
	// for (int substitute : substitutes.keySet()) {
	// if (substitute == i) {
	// continue;
	// }
	// final Map<Variable, int[]> domains = substitutes
	// .get(substitute);
	// boolean substituable = true;
	// for (Variable n : variable.getNeighbours()) {
	// if (!substituable) {
	// break;
	// }
	// final int[] domain1 = n.getBooleanDomain();
	// final int[] domain2 = domains.get(n);
	// for (int j = domain1.length; --j >= 0;) {
	// if ((domain2[j] & domain1[j]) != domain1[j]) {
	// substituable = false;
	// break;
	// }
	// }
	// }
	//
	// if (substituable) {
	// logger.fine(variable + ", " + i + " is substituable !");
	// variable.remove(i, level);
	// nbSub++;
	// changedGraph = true;
	// break;
	// }
	//
	// }
	// }
	// if (changedGraph && !filter.reduceAfter(level, variable)) {
	// return false;
	// }
	// return true;
	// }

	private void reduceToSolution(final int level) {
		for (Variable v : problem.getVariables()) {
			final int sol = v.getFirst();
			v.unassign(problem);
			v.restoreLevel(level + 1);
			v.makeSingletonIndex(sol, level);
		}
	}

	public boolean reduceAfter(final int level, final Variable variable)
			throws OutOfTimeException {
		if (variable == null) {
			return true;
		}
		return reduceAll(level);
	}

	public boolean reduceAll(final int level) throws OutOfTimeException {
		// clearQueue();
		return reduce(level);
	}

	public enum SPACE {
		NONE, CLASSIC, BRANCH
	}

	private boolean buildBranch(final int level, final Variable lastVariable)
			throws OutOfTimeException {
		if (problem.getNbFutureVariables() == 0) {
			return true;
		}

		chronometer.checkExpiration();

		if (lastVariable != null && !filter.reduceAfter(level, lastVariable)) {
			queue[lastVariable.getId()][selectedIndex] = true;
			return false;
		}

		Variable variable = null;

		final boolean[] vQueue = this.vQueue;

		// for (Variable v : problem.getVariables()) {
		// if (!v.isAssigned() && vQueue[v.getId()]
		// && (variable == null || heuristic.compare(v, variable) < 0)) {
		// final int index = getRemainingIndex(v);
		// if (index >= 0) {
		// variable = v;
		// selectedIndex = index;
		// }
		// }
		// }

//		for (Variable v : problem.getVariables()) {
//			if (vQueue[v.getId()] && !v.isAssigned()) {
//				final int index = getRemainingIndex(v);
//				if (index >= 0) {
//					variable = v;
//					selectedIndex = index;
//					break;
//				}
//			}
//		}
		
		for (int v = vQueue.length ; --v >=0;) {
			if (vQueue[v]) {
				final Variable var = problem.getVariable(v) ;
				if (!var.isAssigned()) {
					final int index = getRemainingIndex(var) ;
					if (index >= 0) {
						variable = var;
						selectedIndex = index;
						break;
					}
				}
			}
		}

		if (variable == null) {
			return false;
		}

		selectedVariable = variable;

		queue[variable.getId()][selectedIndex] = false;

		// queue.remove(selectedPair);
		// assert !queue.contains(selectedPair);

		if (logger.isLoggable(Level.FINE)) {
			logger.fine(level + " : " + variable + " ("
					+ variable.getDomainSize() + ") <- "
					+ variable.getDomain()[selectedIndex]);
		}

		variable.assign(selectedIndex, problem);

		problem.setLevelVariables(level, variable.getId());

		// incrementNbAssignments();

		nbSingletonTests++;

		if (buildBranch(level + 1, variable)) {
			return true;
		}

		// selectedVariable.unassign(problem);
		// problem.restore(level + 1);
		//
		// problem.setLevelVariables(level, -1);

		return false;
	}

	private int getRemainingIndex(final Variable variable) {
		final boolean[] iQueue = queue[variable.getId()];
		for (int i = iQueue.length; --i >= 0;) {
			if (iQueue[i] && variable.isPresent(i)) {
				return i;
			}
		}

		return -1;
	}

	public int getNbNoGoods() {
		return nbNoGoods;
	}

	// public int getNbSub() {
	// return nbSub;
	// }

	public int getNbSingletonTests() {
		return nbSingletonTests;
	}

}
