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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.exception.OutOfTimeException;
import cspfj.heuristic.DDegOnDom;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;
import cspfj.util.Maximier;

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

	private final List<Variable> queue;

	private final Map<Integer, SortedSet<Integer>> indexQueues;

	private final Heuristic heuristic;

	public SAC(Problem problem, Chronometer chronometer, Filter filter,
			boolean branch) {
		super();
		this.filter = filter;
		// this.maxNoGoodSize = maxNoGoodSize;
		this.chronometer = chronometer;
		this.problem = problem;
		this.branch = branch;

		queue = new ArrayList<Variable>();

		indexQueues = new HashMap<Integer, SortedSet<Integer>>();
		for (Variable variable : problem.getVariables()) {
			final SortedSet<Integer> indexQueue = new TreeSet<Integer>();
			indexQueues.put(variable.getId(), indexQueue);
		}
		heuristic = new DDegOnDom(problem);
		// queue = new Maximier<Variable>(new
		// Variable[problem.getNbVariables()]);
		// inQueue = new boolean[problem.getNbVariables()];
	}

	private final void fillQueue() {
		final List<Variable> queue = this.queue;
		for (Variable variable : problem.getVariables()) {
			queue.add(variable);
			final SortedSet<Integer> indexQueue = indexQueues.get(variable
					.getId());
			for (int i : variable) {
				indexQueue.add(i);
			}
		}
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

		final List<Variable> queue = this.queue;
		final Map<Integer, SortedSet<Integer>> indexQueues = this.indexQueues;

		fillQueue();

		while (!queue.isEmpty()) {
			final int lastVariableIndex = queue.size() - 1;
			final Variable variable = queue.get(lastVariableIndex);

			if (variable.getDomainSize() <= 1) {
				queue.remove(lastVariableIndex);
				continue;
			}

			final int index = presentIndex(variable, indexQueues);

			if (index < 0) {
				queue.remove(lastVariableIndex);
				continue;
			}

			boolean changedGraph = false;

			if (logger.isLoggable(Level.FINE)) {
				logger.fine(level + " : " + variable + " <- "
						+ +variable.getDomain()[index]);
			}

			variable.assign(index, problem);
			problem.setLevelVariables(level, variable.getId());

			if (filter.reduceAfter(level + 1, variable)) {
				if (branch && buildBranch(level + 1, null)) {
					reduceToSolution(level);
					return true;
				}
				if (problem.addNoGoods() > 0) {
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

	private static int presentIndex(final Variable variable,
			final Map<Integer, SortedSet<Integer>> indexQueues) {
		final SortedSet<Integer> indexQueue = indexQueues.get(variable.getId());

		while (!indexQueue.isEmpty()) {
			final int index = indexQueue.first();
			indexQueue.remove(index);
			if (variable.isPresent(index)) {
				return index;
			}

		}

		return -1;
	}

	private void reduceToSolution(final int level) {
		for (Variable v : problem.getVariables()) {
			final int sol = v.getFirstPresentIndex();
			v.unassign(problem);
			v.restoreLevel(level + 1);
			v.makeSingletonIndex(sol, level);
		}
	}

	private void setValueHeuristic(final Variable variable, final int index) {
		double space = 0;
		for (Variable v : problem.getVariables()) {
			if (v != variable) {
				space += Math.log(v.getDomainSize());
			}
		}
		variable.setValueHeuristic(index, space);
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

	private boolean buildBranch(final int level,
			final Variable lastModifiedVariable) throws OutOfTimeException {
		if (problem.getNbFutureVariables() == 0) {
			return true;
		}

		chronometer.checkExpiration();

		if (!filter.reduceAfter(level, lastModifiedVariable)) {

			return false;
		}

		final Collection<Integer> indexQueue;

		if (lastModifiedVariable == null) {
			indexQueue = null;
		} else {
			indexQueue = indexQueues.get(lastModifiedVariable.getId());

			indexQueue.remove(lastModifiedVariable.getFirstPresentIndex());
		}

		final Variable selectedVariable = heuristic.selectVariable();

		int selectedIndex = -1;

		if (indexQueue != null) {
			for (int i : selectedVariable) {
				if (indexQueue.contains(i)) {
					selectedIndex = i;
					break;
				}
			}
		}

		if (selectedIndex == -1) {
			selectedIndex = selectedVariable.getFirstPresentIndex();
		}

		if (logger.isLoggable(Level.FINER)) {
			logger.finer(level + " : " + selectedVariable + " <- "
					+ selectedVariable.getDomain()[selectedIndex]);
		}

		selectedVariable.assign(selectedIndex, problem);

		problem.setLevelVariables(level, selectedVariable.getId());

		// incrementNbAssignments();

		if (buildBranch(level + 1, selectedVariable)) {
			return true;
		}

		// selectedVariable.unassign(problem);
		// problem.restore(level + 1);
		//
		// problem.setLevelVariables(level, -1);

		return false;
	}

}
