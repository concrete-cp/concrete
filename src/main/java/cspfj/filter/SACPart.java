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

import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public class SACPart extends AbstractSAC {

	// private final static Logger logger =
	// Logger.getLogger("cspfj.filter.CDC");

	private int parts = 10;

	private static final Logger logger = Logger.getLogger(SACPart.class
			.toString());

	public SACPart(Problem problem, Filter filter) {
		super(problem, filter);
	}

	protected boolean singletonTest(final Variable variable, final int level) {
		boolean changedGraph = false;
		final double partSize = (double) variable.getDomainSize() / parts;

		final int[] initialDomain = new int[variable.getDomainSize()];
		for (int i = variable.getFirst(), j = 0; i >= 0; i = variable
				.getNext(i), j++) {
			initialDomain[j] = i;
		}

		for (int part = parts; --part >= 0;) {
			final int partFirst = (int) (part * partSize);
			final int partLast = (int) ((part + 1) * partSize) - 1;

			if (partLast < partFirst) {
				continue;
			}

			if (testPart(variable, level, initialDomain, partFirst, partLast)) {
				changedGraph = true;
			}

		}

		return changedGraph;
	}

	protected boolean testPart(final Variable variable, final int level,
			final int[] domain, final int first, final int last) {
		logger.finer("Level " + level + " - " + variable + ": " + first + "-"
				+ last);
		for (int i = domain.length; --i >= 0;) {
			if (i < first || i > last) {
				if (variable.isPresent(domain[i])) {
					variable.remove(domain[i], level + 1);
				}
			}
		}
		nbSingletonTests++;
		final boolean test = filter.reduceAfter(level + 1, variable);

		problem.restore(level + 1);

		if (!test) {
			for (int i = first; i <= last; i++) {
				if (variable.isPresent(domain[i])) {
					variable.remove(domain[i], level);
				}
			}
			return true;
		}
		return false;

	}

	public String toString() {
		return "SAC-parts w/ " + filter;
	}

}
