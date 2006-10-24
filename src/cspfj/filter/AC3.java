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

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {

	// private static final Logger logger = Logger.getLogger("cspfj.AC3");

	private final Problem problem;

	// private final Maximier<Variable> queue;

	private final boolean[] inQueue;

	private int queueSize = 0;

	// private final Map<Integer, boolean[]> inQueue;

	public AC3(final Problem problem) {
		super();
		this.problem = problem;

		// inQueue = new HashMap<Integer,
		// boolean[]>(problem.getNbConstraints());

		// int size = 0;
		// for (Constraint c : problem.getConstraints()) {
		// size += c.getArity();
		// inQueue.put(c.getId(), new boolean[c.getArity()]);
		// }

		int maxId = -1;

		for (Variable v : problem.getVariables()) {
			if (v.getId() > maxId) {
				maxId = v.getId();
			}
		}

		inQueue = new boolean[maxId + 1];

		// queue = new Maximier<Variable>(new
		// Variable[problem.getNbVariables()]);
	}

	public boolean reduceAll(final int level) {
		clearQueue();
		addAll();
		return reduce(level);

	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		clearQueue();
		addNeighbours(variable);

		return reduce(level);
	}

	private boolean reduce(final int level) {
		while (queueSize > 0) {
			// System.err.println(queueSize);
			final Variable variable = pullVariable();

			boolean revised = false;

			for (Constraint c : variable.getInvolvingConstraints()) {
				if (c.revise(variable, level)) {

					if (variable.getDomainSize() <= 0) {
						c.increaseWeight();
						return false;
					}

					revised = true;
				}
			}
			if (revised) {
				addNeighbours(variable);

			}

		}

		return true;

	}

	private void clearQueue() {
		final boolean[] inQueue = this.inQueue;
		for (int i = inQueue.length; --i >= 0;) {
			inQueue[i] = false;
		}
		queueSize = 0;
		// queue.clear();
	}

	private void addAll() {

		for (Variable v : problem.getVariables()) {
			addInQueue(v);

		}

	}

	private Variable pullVariable() {
		// final Variable variable = queue.pull();

		Variable variable = null;

		final boolean[] inQueue = this.inQueue;

		final Problem problem = this.problem;

		for (int i = inQueue.length; --i >= 0;) {
			if (inQueue[i]
					&& (variable == null || problem.getVariable(i)
							.getDomainSize() < variable.getDomainSize())) {
				variable = problem.getVariable(i);
			}
		}

		inQueue[variable.getId()] = false;
		queueSize--;
		return variable;
	}

	private void addInQueue(final Variable variable) {
		if (variable.getDomainSize() > 1 && !inQueue[variable.getId()]) {
			inQueue[variable.getId()] = true;
			queueSize++;
		}

	}

	private void addNeighbours(final Variable variable) {
		for (Variable n : variable.getNeighbours()) {
			addInQueue(n);

		}

	}

	public int getNbNoGoods() {
		return 0;
	}

	public int getNbSub() {
		return 0;
	}

}
