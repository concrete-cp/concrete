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

import cspfj.exception.OutOfTimeException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;

/**
 * @author Julien VION
 * 
 */
public class SAC implements Filter {

    private final Filter filter;

    private final int maxNoGoodSize;

    private final static Logger logger = Logger.getLogger("cspfj.filter.SAC");

    private final Chronometer chronometer;

    private final Problem problem;

    // private final boolean[] inQueue;

    // private final Maximier<Variable> queue;

    public SAC(Problem problem, int maxNoGoodSize, Chronometer chronometer,
            Filter filter) {
        super();
        this.filter = filter;
        this.maxNoGoodSize = maxNoGoodSize;
        this.chronometer = chronometer;
        this.problem = problem;
        // queue = new Maximier<Variable>(new
        // Variable[problem.getNbVariables()]);
        // inQueue = new boolean[problem.getNbVariables()];
    }

    private boolean reduce(final int level) throws OutOfTimeException {
        logger.info("SAC");

        final Filter filter = this.filter;

        if (!filter.reduceAll(level)) {
            return false;
        }

        final Problem problem = this.problem;

        //
        // boolean changed;
        //
        // do {
        // changed = false;
        // addAll();
        // logger.fine("NEW TURN !!");
        // while (!queue.isEmpty()) {
        for (Variable variable : problem.getVariables()) {

            if (variable.getDomainSize() <= 1) {
                continue;
            }

            for (int i : variable) {
                if (logger.isLoggable(Level.FINE)) {
                    logger.fine(variable + " <- " + i);
                }
                chronometer.checkExpiration();
                boolean changedGraph = false;
                variable.assign(i, problem);
                problem.setLevelVariables(level, variable.getId());

                if (filter.reduceAfter(level + 1, variable)) {

                    if (problem.addNoGoods(maxNoGoodSize) > 0) {
                        // changed = true;
                        changedGraph = true;
                    }

                    variable.unassign(problem);
                    problem.restore(level + 1);

                } else {

                    variable.unassign(problem);
                    problem.restore(level + 1);

                    variable.remove(i, level);
                    if (variable.getDomainSize() <= 0) {
                        return false;
                    }
                    changedGraph = true;
                }

                if (changedGraph && !filter.reduceAfter(level, variable)) {
                    return false;
                }
            }

        }
        problem.setLevelVariables(level, -1);
        // } while (changed);

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
        // clearQueue();
        return reduce(level);
    }

    // private boolean addInQueue(final Variable variable) {
    // if (variable.getDomainSize() > 1 && !inQueue[variable.getId()]) {
    // queue.add(variable);
    // inQueue[variable.getId()] = true;
    // return true;
    // }
    // return false;
    // }
    //
    // private void clearQueue() {
    // queue.clear();
    // Arrays.fill(inQueue, false);
    // }
    //
    // private Variable pullVariable() {
    // final Variable variable = queue.pull();
    // inQueue[variable.getId()] = false;
    // return variable;
    // }
    //
    // private void addVariables(final Variable[] variables) {
    // boolean change = false;
    // for (Variable v : variables) {
    // change |= addInQueue(v);
    // }
    // if (change) {
    // queue.sort();
    // }
    // }
    //
    // private void addAll() {
    // addVariables(problem.getVariables());
    // }
}
