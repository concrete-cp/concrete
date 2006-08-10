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

import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Arc;
import cspfj.util.Maximier;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {

    private static final Logger logger = Logger.getLogger("cspfj.AC3");

    private final Problem problem;

    private final Maximier<Arc> queue;

    private final boolean[][] inQueue;

    public AC3(final Problem problem) {
        super();
        this.problem = problem;

        inQueue = new boolean[problem.getNbConstraints()][];

        int size = 0;
        for (int i = 0; i < problem.getNbConstraints(); i++) {
            size += problem.getConstraints()[i].getArity();
            inQueue[i] = new boolean[problem.getConstraints()[i].getArity()];
        }

        queue = new Maximier<Arc>(new Arc[size]);
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
        logger.finer("Filtering...");

        while (!queue.isEmpty()) {
            final Arc arc = pullQueue();

            // logger.finest("Revising variable " + variable + "("
            // + variable.getDomainSize() + ")");

            boolean revised = false;

            logger.finest(arc.getVariable() + ", " + arc.getConstraint());
            if (arc.revise(level)) {

                if (arc.getVariable().getDomainSize() < 1) {
                    arc.getConstraint().increaseWeight();
                    return false;
                }

                revised = true;
            }

            if (revised) {
                addNeighbours(arc.getVariable());

            }

        }

        return true;

    }

    private void clearQueue() {
        for (boolean[] i : inQueue) {
            for (int j = 0; j < i.length; j++) {
                i[j] = false;
            }
        }
        queue.clear();
    }

    private void addAll() {
        boolean change = false;

        for (Constraint c : problem.getConstraints()) {
            for (Arc a : c.getArcs()) {
//                logger.finest("Adding " + a);
                if (addInQueue(a)) {
                    change = true;
                } else {
//                    logger.finest("Not added !");
                }
            }
        }
        if (change) {
            queue.sort();
        }
    }

    private Arc pullQueue() {
        final Arc arc = queue.pull();
        inQueue[arc.getCId()][arc.getPosition()] = false;
        return arc;
    }

    private boolean addInQueue(final Arc arc) {
        if (arc.getVariable().getDomainSize() <= 1) {
            return false;
        }
        if (inQueue[arc.getCId()][arc.getPosition()]) {
            return false;
        }
        queue.add(arc);
        inQueue[arc.getCId()][arc.getPosition()] = true;
        return true;
    }

    private void addNeighbours(final Variable variable) {
        boolean change = false;
        for (Constraint c : variable.getInvolvingConstraints()) {
            for (Arc a : c.getArcs()) {
                if (a.getVariable() == variable) {
                    continue;
                }
                if (addInQueue(a)) {
                    change = true;
                }
            }
        }
        if (change) {
            queue.sort();
        }
    }

}
