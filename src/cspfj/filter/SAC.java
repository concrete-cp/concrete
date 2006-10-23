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

import java.util.Iterator;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.exception.OutOfTimeException;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

/**
 * @author Julien VION
 * 
 */
public class SAC implements Filter {

	private final Filter filter;

	// private final int maxNoGoodSize;

	private final static Logger logger = Logger.getLogger("cspfj.filter.SAC");

	private final Chronometer chronometer;

	private final Problem problem;

	private final boolean branch;

	// private final boolean[] inQueue;

	private final SortedSet<Pair> queue;

	// private final Map<Integer, SortedSet<Integer>> indexQueues;

	private int nbNoGoods = 0;

	private int nbSub = 0;

	public SAC(Problem problem, Chronometer chronometer, Filter filter,
			boolean branch) {
		super();
		this.filter = filter;
		// this.maxNoGoodSize = maxNoGoodSize;
		this.chronometer = chronometer;
		this.problem = problem;
		this.branch = branch;

		queue = new TreeSet<Pair>();

		Pair.setHeuristic(new WDegOnDom(problem));
		// indexQueues = new HashMap<Integer, SortedSet<Integer>>();
		// for (Variable variable : problem.getVariables()) {
		// final SortedSet<Integer> indexQueue = new TreeSet<Integer>();
		// indexQueues.put(variable.getId(), indexQueue);
		// }

		// queue = new Maximier<Variable>(new
		// Variable[problem.getNbVariables()]);
		// inQueue = new boolean[problem.getNbVariables()];
	}

	private final void fillQueue() {
		final SortedSet<Pair> queue = this.queue;
		for (Variable variable : problem.getVariables()) {
			for (int i = variable.getFirst(); i >= 0; i = variable
					.getNext(i)) {
				queue.add(new Pair(variable, i));
			}
		}
	}

	private final Pair pull() {
		final Iterator<Pair> i = queue.iterator();

		final Pair selectedPair = i.next();
		i.remove();

		return selectedPair;
	}

	private boolean reduce(final int level) throws OutOfTimeException {
		final Problem problem = this.problem;

		if (logger.isLoggable(Level.INFO)) {
			int cpt = 0;
			for (Variable variable : problem.getVariables()) {
				if (variable.getDomainSize() > 1) {
					cpt++;
				}
			}

			logger.info("SAC : " + cpt + " future variables / "
					+ problem.getNbVariables());

		}

		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}

		final SortedSet<Pair> queue = this.queue;
		// final Map<Integer, SortedSet<Integer>> indexQueues =
		// this.indexQueues;

		// final Map<Integer, Map<Variable, int[]>> substitutes = new
		// HashMap<Integer,
		// Map<Variable, int[]>>();

		fillQueue();

		while (!queue.isEmpty()) {
			final Pair pair = pull();
			final Variable variable = pair.getVariable();
			final int index = pair.getValue();

			if (!variable.isPresent(index)) {
				continue;
			}

			// if (variable.getDomainSize() <= 1) {
			// // if (!substitute(variable, substitutes, level)) {
			// // return false;
			// // }
			// substitutes.clear();
			// continue;
			// }

			boolean changedGraph = false;

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(level + " : " + variable + " <- "
						+ variable.getDomain()[index]);
			}

			variable.assign(index, problem);
			problem.setLevelVariables(level, variable.getId());

			if (filter.reduceAfter(level + 1, variable)) {
				// final Map<Variable, int[]> domains = new HashMap<Variable,
				// int[]>();

				// substitutes.put(index, domains);
				//
				// for (Variable n : variable.getNeighbours()) {
				// domains.put(n, n.getBooleanDomain().clone());
				// }

				if (branch && buildBranch(level + 1, null)) {
					reduceToSolution(level);
					return true;
				}

				final int nbNg = problem.addNoGoods();
				nbNoGoods += nbNg;
				if (nbNg > 0) {
					changedGraph = true;
				}

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

	private boolean substitute(final Variable variable,
			final Map<Integer, Map<Variable, int[]>> substitutes,
			final int level) throws OutOfTimeException {
		boolean changedGraph = false;
		for (int i = variable.getFirst(); i >= 0; i = variable
				.getNext(i)) {
			for (int substitute : substitutes.keySet()) {
				if (substitute == i) {
					continue;
				}
				final Map<Variable, int[]> domains = substitutes
						.get(substitute);
				boolean substituable = true;
				for (Variable n : variable.getNeighbours()) {
					if (!substituable) {
						break;
					}
					final int[] domain1 = n.getBooleanDomain();
					final int[] domain2 = domains.get(n);
					for (int j = domain1.length; --j >= 0;) {
						if ((domain2[j] & domain1[j]) != domain1[j]) {
							substituable = false;
							break;
						}
					}
				}

				if (substituable) {
					logger.fine(variable + ", " + i + " is substituable !");
					variable.remove(i, level);
					nbSub++;
					changedGraph = true;
					break;
				}

			}
		}
		if (changedGraph && !filter.reduceAfter(level, variable)) {
			return false;
		}
		return true;
	}

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

	private boolean buildBranch(final int level, final Pair lastPair)
			throws OutOfTimeException {
		if (problem.getNbFutureVariables() == 0) {
			return true;
		}

		chronometer.checkExpiration();

		if (lastPair != null
				&& !filter.reduceAfter(level, lastPair.getVariable())) {
			queue.add(lastPair);
			return false;
		}

		Pair selectedPair = null;

		final Iterator<Pair> i = queue.iterator();

		while (i.hasNext()) {
			final Pair pair = i.next();
			if (!pair.getVariable().isAssigned()
					&& pair.getVariable().isPresent(pair.getValue())) {
				selectedPair = pair;
				i.remove();
				break;
			}
		}

		if (selectedPair == null) {
			return false;
		}

		// queue.remove(selectedPair);
		// assert !queue.contains(selectedPair);

		final Variable selectedVariable = selectedPair.getVariable();

		final int selectedIndex = selectedPair.getValue();

		if (logger.isLoggable(Level.FINE)) {
			logger.fine(level + " : " + selectedVariable + " ("
					+ selectedVariable.getDomainSize() + ") <- "
					+ selectedVariable.getDomain()[selectedIndex]);
		}

		selectedVariable.assign(selectedIndex, problem);

		problem.setLevelVariables(level, selectedVariable.getId());

		// incrementNbAssignments();

		if (buildBranch(level + 1, selectedPair)) {
			return true;
		}

		// selectedVariable.unassign(problem);
		// problem.restore(level + 1);
		//
		// problem.setLevelVariables(level, -1);

		return false;
	}

	public int getNbNoGoods() {
		return nbNoGoods;
	}

	public int getNbSub() {
		return nbSub;
	}

}
