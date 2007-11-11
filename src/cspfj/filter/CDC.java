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
public final class CDC implements Filter {

	private final Filter filter;

	private final static Logger logger = Logger.getLogger("cspfj.filter.CDC");

	private final Problem problem;

	private int nbNoGoods = 0;

	private int nbSingletonTests = 0;

	public CDC(Problem problem, Filter filter) {
		super();
		this.filter = filter;
		this.problem = problem;
	}

	private int next(final int i, final int length) {
		if (i < length - 1) {
			return i + 1;
		}
		logger.info("Tour !");
		return 0;
	}

	private boolean reduce(final int level) {
		final Problem problem = this.problem;

		logger.info("CDC");

		final Filter filter = this.filter;

		if (!filter.reduceAll(level)) {
			return false;
		}

		final Variable[] variables = problem.getVariables();

		int mark = 0;

		int v = 0;

		do {
			final Variable variable = variables[v];

			for (int index = variable.getFirst(); index >= 0; index = variable
					.getNext(index)) {

				boolean changedGraph = false;
				
				if (!variable.isPresent(index)) {
					continue;
				}

				if (logger.isLoggable(Level.FINE)) {
					logger.fine(level + " : " + variable + " <- "
							+ variable.getDomain()[index] + "(" + index + ")");
				}

				variable.assign(index, problem);
				problem.setLevelVariables(level, variable);
				nbSingletonTests++;
				if (filter.reduceAfter(level + 1, variable)) {
					final int nbNg = problem.addNoGoods();
					nbNoGoods += nbNg;

					changedGraph = nbNg > 0;

					variable.unassign(problem);
					problem.restore(level + 1);
				} else {
					variable.unassign(problem);
					problem.restore(level + 1);
					logger.fine("Removing " + variable + ", " + index);

					variable.remove(index, level);
					changedGraph = true;

					if (variable.getDomainSize() <= 0) {
						return false;
					}
				}

				if (changedGraph) {
					if (!filter.reduceAfter(level, variable)) {
						return false;
					}
					mark = v;
				}
			}
			v = next(v, variables.length);
		} while (v != mark);
//		logger.info("Total : " + Problem.noGoodTime);
//		logger.info("Find Constraint : " + Problem.findConstraintTime);
//		logger.info("Remove Tuples : " + Problem.removeTupleTime);
		return true;

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
		return "CDC";
	}
}
