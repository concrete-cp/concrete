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

import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class B3C implements Filter {

	private final Filter filter;

	private final static Logger logger = Logger.getLogger("cspfj.filter.CDC");

	private final Problem problem;

	private int nbNoGoods = 0;

	private int nbSingletonTests = 0;

	private final Variable[] variables;

	public B3C(Problem problem, Filter filter) {
		super();
		this.filter = filter;
		this.problem = problem;
		variables = problem.getVariables().clone();
	}

	private int next(final int i, final int start, final int length) {
		if (i < length - 1) {
			return i + 1;
		}
		logger.info("Tour !");
		return start;
	}

	private boolean reduce(final int level) {
		logger.info("B3C @ level " + level);

		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}

		final Variable[] variables = this.variables;

		for (int i = 0; i < variables.length; i++) {
			boolean changed = false;
			for (int j = variables.length; --j > i;) {
				if (variables[j].getDomainSize() < variables[j - 1]
						.getDomainSize()) {
					final Variable temp = variables[j];
					variables[j] = variables[j - 1];
					variables[j - 1] = temp;
					changed = true;
				}
			}
			if (!changed) {
				break;
			}
		}

		int start = 0;

		while (variables[start].getDomainSize() <= 1) {

			start++;
			if (start >= variables.length) {
				return true;
			}

		}

		int mark = start;
		int v = start;

		do {
			final Variable variable = variables[v];
			if (variable.getDomainSize() > 1) {

				for (int index = variable.getFirst(); index >= 0; index = variable
						.getNext(index)) {
					if (!variable.isPresent(index)) {
						continue;
					}
					if (check(variable, index, level)) {
						if (variable.getDomainSize() == 0) {
							return false;
						}
						if (!filter.reduceAfter(level, variable)) {
							return false;
						}
						mark = v;
					} else {
						break;
					}
				}
				if (variable.getDomainSize() > 1) {
					for (int index = variable.getLast(); index >= 0; index = variable
							.getPrev(index)) {
						if (!variable.isPresent(index)) {
							continue;
						}
						if (check(variable, index, level)) {
							if (variable.getDomainSize() == 0) {
								return false;
							}
							if (!filter.reduceAfter(level, variable)) {
								return false;
							}
							mark = v;
						} else {
							break;
						}
					}
				}
			}
			v = next(v, start, variables.length);
		} while (v != mark);
		// logger.info("Total : " + Problem.noGoodTime);
		// logger.info("Find Constraint : " + Problem.findConstraintTime);
		// logger.info("Remove Tuples : " + Problem.removeTupleTime);
		return true;

	}

	private boolean check(Variable variable, int index, int level) {
		if (logger.isLoggable(Level.FINE)) {
			logger.fine(level + " : " + variable + " <- "
					+ variable.getDomain()[index] + "(" + index + ")");
		}

		variable.assign(index, problem);
		problem.setLevelVariables(level, variable);
		nbSingletonTests++;
		final boolean singletonTest = filter.reduceAfter(level + 1, variable);
		variable.unassign(problem);
		problem.restore(level + 1);

		if (!singletonTest) {
			logger.fine("Removing " + variable + ", " + index);

			variable.remove(index, level);
			return true;
		}

		return false;
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		return reduceAll(level);
	}

	public boolean reduceAll(final int level) {
		return reduce(level);
	}

	public int getNbNoGoods() {
		return nbNoGoods;
	}

	public int getNbSingletonTests() {
		return nbSingletonTests;
	}

	public String toString() {
		return "3B";
	}
}
