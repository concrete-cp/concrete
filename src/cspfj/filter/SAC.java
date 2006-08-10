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

import cspfj.exception.OutOfTimeException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

/**
 * @author Julien VION
 *
 */
public class SAC extends AbstractFilter {

    private final Filter filter;

    private final int maxNoGoodSize;

    private final static Logger logger = Logger.getLogger("cspfj.filter.SAC");

    private final Chronometer chronometer;

    public SAC(Problem problem, int maxNoGoodSize, Chronometer chronometer,
            Filter filter) {
        super(problem);
        this.filter = filter;
        this.maxNoGoodSize = maxNoGoodSize;
        this.chronometer = chronometer;
    }

    private boolean reduce(final int level) throws OutOfTimeException {
        logger.info("SAC");
        if (!filter.reduceAll(level)) {
            return false;
        }

        boolean changed;

        do {
            changed = false;
            addAll();
            logger.fine("NEW TURN !!");
            while (!isQueueEmpty()) {

                final Variable variable = pullVariable();
                if (variable.getDomainSize() <= 1) {
                    continue;
                }
                logger.fine(variable + "(" + queueSize() + " left)");
                for (int i : variable) {

                    chronometer.checkExpiration();

                    boolean changedValue = false;

                    variable.assign(i, problem);
                    problem.setLevelVariables(level, variable.getId());
                    if (filter.reduceAfter(level + 1, variable)) {

                        if (problem.addNoGoods(maxNoGoodSize) > 0) {
                            changed = changedValue = true;

                        }
                        variable.unassign(problem);

                    } else {
                        logger.fine("Removing");
                        variable.unassign(problem);
                        variable.remove(i, level);
                        changedValue = true;

                    }
                    problem.restore(level + 1);

                    if (changedValue
                            && (variable.getDomainSize() < 1 || !filter
                                    .reduceAfter(level, variable))) {
                        return false;
                    }

                }

            }
            problem.setLevelVariables(level, -1);
        } while (changed);

        return true;

    }

    public boolean reduceAfter(final int level, final Variable variable)
            throws OutOfTimeException {
        if (variable == null) {
            return true;
        }
        return reduceAll(level);
    }

    public boolean reduceAll(final int level) throws OutOfTimeException {
        clearQueue();
        return reduce(level);
    }
}
