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

import java.util.HashMap;
import java.util.Map;

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author scand1sk
 * 
 */
public final class FC implements Filter {
	public int nbEffectiveRevisions = 0;

	public int nbUselessRevisions = 0;

	public FC(final Problem problem) {
		super();
	}

	public boolean reduceAll(final int level) {
		return true;
	}

	public boolean reduceAfter(final int level, final Variable variable) {
		if (variable == null) {
			return true;
		}
		if (variable.getDomainSize()==0) {
			return false;
		}
		final Constraint[] constraints = variable.getInvolvingConstraints();

		for (int c = constraints.length; --c >= 0;) {

			final Constraint constraint = constraints[c];

			for (int i = constraint.getArity(); --i >= 0;) {
				final Variable y = constraint.getInvolvedVariables()[i];

				if (!y.isAssigned()) {
					if (constraint.revise(i, level)) {
						nbEffectiveRevisions++;
						if (y.getDomainSize() <= 0) {
							constraint.increaseWeight();
							return false;
						}
					} else {
						nbUselessRevisions++;
					}

				}
			}

		}
		return true;

	}

	public String toString() {
		return "FC";
	}

	public Map<String, Object> getStatistics() {
		final Map<String, Object> statistics = new HashMap<String, Object>();
		statistics.put("fc-effective-revisions", nbEffectiveRevisions);
		statistics.put("fc-useless-revisions", nbUselessRevisions);
		return statistics;
	}

}
