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
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.problem.Variable;

/**
 * @author Julien VION
 * 
 */
public final class B3C extends AbstractSAC {
    private final static Logger LOGGER = Logger.getLogger(B3C.class.getName());

    // private final static Logger logger =
    // Logger.getLogger("cspfj.filter.CDC");

    public B3C(Problem problem) {
        super(problem, new AC3(problem));
    }

    @Override
    protected boolean singletonTest(final Variable variable) {
        boolean changed = false;
        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {
            if (!variable.isPresent(index)) {
                continue;
            }
            if (check(variable, index)) {
                changed = true;
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
                if (check(variable, index)) {
                    changed = true;
                } else {
                    break;
                }
            }
        }
        return changed;
    }

    private boolean check(Variable variable, int index) {
        problem.push();
        variable.assign(index);
        problem.setCurrentLevelVariable(variable);
        nbSingletonTests++;
        final boolean consistent = filter.reduceAfter(variable);
        variable.unassign();
        problem.pop();

        if (!consistent) {
            LOGGER.fine("Removing " + variable + ", " + index);

            variable.remove(index);
            return true;
        }
        return false;
    }

    public String toString() {
        return "3B w/ " + filter;
    }

    public Map<String, Object> getStatistics() {
        final Map<String, Object> statistics = new HashMap<String, Object>();
        statistics.put("3B-singletonTests", nbSingletonTests);
        for (Entry<String, Object> stat : filter.getStatistics().entrySet()) {
            statistics.put("3B-backend-" + stat.getKey(), stat.getValue());
        }
        return statistics;
    }

}
