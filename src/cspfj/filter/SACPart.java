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

import java.util.Map;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class SACPart extends AbstractSAC {

	// private final static Logger logger =
	// Logger.getLogger("cspfj.filter.CDC");

	private final static double PARTS = 10;

	public SACPart(Problem problem, Filter filter) {
		super(problem, filter);
	}

	protected boolean singletonTest(final Variable variable, final int level) {
		boolean changedGraph = false;
		final double partSize = variable.getDomainSize() / PARTS;

		for (int part = (int) PARTS; --part >= 0;) {
			final int partFirst = (int) (part * partSize);
			final int partLast = (int) ((part + 1) * partSize) - 1;

			if (partLast < partFirst) {
				continue;
			}

			for (int i = variable.getFirst(), j = 0; i >= 0; i = variable
					.getNext(i), j++) {
				if (j < partFirst || j > partLast) {
					variable.remove(i, level + 1);
				}
			}
			nbSingletonTests++;
			final boolean test = filter.reduceAfter(level + 1, variable);

			problem.restore(level + 1);

			if (!test) {
				for (int i = variable.getFirst(), j = 0; i >= 0; i = variable
						.getNext(i), j++) {
					if (j > partLast) {
						break;
					}
					if (j >= partFirst) {
						variable.remove(i, level);
					}

				}

				changedGraph = true;
			}
		}

		return changedGraph;
	}

	public String toString() {
		return "CDC";
	}


}
