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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Arc;
import cspfj.constraint.Constraint;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Maximier;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {

    private static final Logger logger = Logger.getLogger("cspfj.AC3");

    private final Problem problem;

    private final Maximier<Arc> queue;

    private final Map<Integer, boolean[]> inQueue;

    public AC3(final Problem problem) {
        super();
        this.problem = problem;

        inQueue = new HashMap<Integer, boolean[]>(problem.getNbConstraints());

        int size = 0;
        for (Constraint c : problem.getConstraints()) {
            size += c.getArity();
            inQueue.put(c.getId(), new boolean[c.getArity()]);
        }

        queue = new Maximier<Arc>(new Arc[size]);
    }

    public boolean reduceAll(final int level) {
        clearQueue();
        addAll(this.queue, this.inQueue);
        return reduce(level);

    }

    public boolean reduceAfter(final int level, final Variable variable) {
        if (variable == null) {
            return true;
        }
        clearQueue();
        addNeighbours(variable, this.queue, this.inQueue);

        return reduce(level);
    }

    private boolean reduce(final int level) {
        final Maximier<Arc> queue = this.queue;
        final Map<Integer, boolean[]> inQueue = this.inQueue;
        while (!queue.isEmpty()) {
            final Arc arc = pullArc(queue, inQueue);

            boolean revised = false;

            if (logger.isLoggable(Level.FINEST)) {
                logger.finest(arc.getVariable() + ", " + arc.getConstraint()
                        + " (" + queue.size() + " arcs restants)");
            }
            if (arc.revise(level)) {

                if (arc.getVariable().getDomainSize() <= 0) {
                    arc.getConstraint().increaseWeight();
                    return false;
                }

                revised = true;
            }

            if (revised) {
                addNeighbours(arc.getVariable(), queue, inQueue);

            }

        }

        return true;

    }

    private void clearQueue() {
        for (boolean[] i : inQueue.values()) {
            Arrays.fill(i, false);
        }
        queue.clear();
    }

    private void addAll(final Maximier<Arc> queue, final Map<Integer, boolean[]> inQueue) {
        boolean change = false;

        for (Constraint c : problem.getConstraints()) {
            for (Arc a : c.getArcs()) {
                if (addInQueue(a, queue, inQueue)) {
                    change = true;
                }
            }
        }
        if (change) {
            queue.sort();
        }
    }

    private static Arc pullArc(final Maximier<Arc> queue,
            final Map<Integer, boolean[]> inQueue) {
        final Arc arc = queue.pull();
        inQueue.get(arc.getCId())[arc.getPosition()] = false;
        return arc;
    }

    private static boolean addInQueue(final Arc arc, final Maximier<Arc> queue,
            final Map<Integer, boolean[]> inQueue) {
        if (arc.getVariable().getDomainSize() <= 1) {
            return false;
        }
        if (inQueue.get(arc.getCId())[arc.getPosition()]) {
            return false;
        }
        queue.add(arc);
        inQueue.get(arc.getCId())[arc.getPosition()] = true;
        return true;
    }

    private static void addNeighbours(final Variable variable,
            final Maximier<Arc> queue, final Map<Integer, boolean[]> inQueue) {
        boolean change = false;
        for (Constraint c : variable.getInvolvingConstraints()) {
            for (Arc a : c.getArcs()) {
                if (a.getVariable() == variable) {
                    continue;
                }
                if (addInQueue(a, queue, inQueue)) {
                    change = true;
                }
            }
        }
        if (change) {
            queue.sort();
        }
    }

}
