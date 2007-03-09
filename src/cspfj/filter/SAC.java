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

import cspfj.heuristic.VariableHeuristic;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class SAC implements Filter {

	private final Filter filter;

	// private final int maxNoGoodSize;

	private final static Logger logger = Logger.getLogger("cspfj.filter.SAC");

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

	private final VariableHeuristic heuristic;

	public SAC(Problem problem, Filter filter, boolean branch) {
		super();
		this.filter = filter;
		// this.maxNoGoodSize = maxNoGoodSize;
		this.problem = problem;
		this.branch = branch;

		queue = new boolean[problem.getMaxVId() + 1][];
		vQueue = new boolean[problem.getMaxVId() + 1];
		for (Variable v : problem.getVariables()) {
			queue[v.getId()] = new boolean[v.getDomain().length];
		}

		heuristic = new WDegOnDom();

		staticVOrder = new Variable[problem.getNbVariables()];

		Arrays.fill(staticVOrder, null);

		int cpt = 0;
		for (Variable v : problem.getVariables()) {
			staticVOrder[cpt++] = v;
		}
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
		for (Variable variable : problem.getVariables()) {
			final int id = variable.getId();
			if (variable.getDomainSize() > 1) {
				vQueue[id] = true;
				Arrays.fill(queue[id], false);
				for (int i = variable.getFirst(); i != -1; i = variable
						.getNext(i)) {
					queue[id][i] = true;
				}

			} else {
				vQueue[id] = false;
			}

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

	private boolean reduce(final int level) {
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

				if (branch) {
					if (buildBranch(level + 1)) {
						reduceToSolution(level);
						return true;
					}
				}

				final int nbNg = problem.addNoGoods();
				nbNoGoods += nbNg;

				changedGraph = nbNg > 0;

				// for (int l = level + 1; l < problem.getNbVariables(); l++) {
				// problem.setLevelVariables(l, -1);
				// }

				variable.unassign(problem);
				problem.restore(level + 1);
			} else {
				variable.unassign(problem);
				problem.restore(level + 1);

				variable.remove(index, level);
				changedGraph = true;

				if (variable.getDomainSize() <= 0) {
					return false;
				}
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

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		return reduceAll(level);
	}

	public boolean reduceAll(final int level) {
		// clearQueue();
		return reduce(level);
	}

	public enum SPACE {
		NONE, CLASSIC, BRANCH
	}

	private final Variable[] staticVOrder;

	private boolean buildBranch(final int level) {
		final Variable[] staticVOrder = this.staticVOrder;
		final boolean[] vQueue = this.vQueue;

		Arrays.sort(staticVOrder, heuristic);

		int l = level;

		while (problem.getNbFutureVariables() > 0) {
			Variable variable;
			int index;
			int cpt = 0;
			do {
				do {
					if (cpt >= staticVOrder.length) {
						for (int k = l; --k >= level;) {
							problem.getVariable(problem.getLevelVariable(k))
									.unassign(problem);
							problem.setLevelVariables(k, -1);
						}
						return false;
					}
					variable = staticVOrder[cpt++];
				} while (!vQueue[variable.getId()]);

				index = getRemainingIndex(variable);
			} while (index < 0);

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(l + " : " + variable + " ("
						+ variable.getDomainSize() + ") <- "
						+ variable.getDomain()[index]);
			}

			// queue.remove(selectedPair);
			// assert !queue.contains(selectedPair);

			variable.assign(index, problem);

			problem.setLevelVariables(l++, variable.getId());

			nbSingletonTests++;

			if (!filter.reduceAfter(l, variable)) {
				selectedVariable = variable;
				selectedIndex = index;
				for (int k = l; --k >= level;) {
					problem.getVariable(problem.getLevelVariable(k)).unassign(
							problem);
					problem.setLevelVariables(k, -1);
				}
				return false;
			}

			queue[variable.getId()][index] = false;
		}

		return true;
	}

	private int getRemainingIndex(final Variable variable) {
		final int vid = variable.getId();
		final boolean[] iQueue = queue[vid];
		for (int i = iQueue.length; --i >= 0;) {
			if (iQueue[i] && variable.isPresent(i)) {
				return i;
			}
		}
		vQueue[vid] = false;
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

	public String toString() {
		return "SAC-" + (branch ? '3' : '1');
	}
}
