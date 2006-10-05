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

package cspfj.heuristic;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class WDegOnDom extends AbstractVariableHeuristic {
    public WDegOnDom(Problem problem) {
		super(problem);
	}

	public int compare(final Variable variable0, final Variable variable1) {
        final float result = variable1.getWDeg() / variable1.getDomainSize()
                - variable0.getWDeg() / variable0.getDomainSize();
        if (result == 0) {
            return variable0.getId() - variable1.getId();
        }
        return result > 0 ? 1 : -1;
    }

}
