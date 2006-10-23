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
import cspfj.util.Maximier;

/**
 * @author scand1sk
 * 
 */
public final class AC3 implements Filter {

    //private static final Logger logger = Logger.getLogger("cspfj.AC3");

    private final Problem problem;

    private final Maximier<Variable> queue;

    private final Map<Integer, Boolean> inQueue;

    // private final Map<Integer, boolean[]> inQueue;

    public AC3(final Problem problem) {
        super();
        this.problem = problem;

        // inQueue = new HashMap<Integer,
        // boolean[]>(problem.getNbConstraints());

        // int size = 0;
        // for (Constraint c : problem.getConstraints()) {
        // size += c.getArity();
        // inQueue.put(c.getId(), new boolean[c.getArity()]);
        // }

        inQueue = new HashMap<Integer, Boolean>(problem.getNbVariables());

        queue = new Maximier<Variable>(new Variable[problem.getNbVariables()]);
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
        final Maximier<Variable> queue = this.queue;
        final Map<Integer, Boolean> inQueue = this.inQueue;
        while (!queue.isEmpty()) {
            final Variable variable = pullVariable(queue, inQueue);

            boolean revised = false;

            for (Constraint c : variable.getInvolvingConstraints()) {
                if (c.revise(variable, level)) {

                    if (variable.getDomainSize() <= 0) {
                        c.increaseWeight();
                        return false;
                    }

                    revised = true;
                }
            }
            if (revised) {
                addNeighbours(variable, queue, inQueue);

            }

        }

        return true;

    }

    private void clearQueue() {
        for (Variable v : problem.getVariables()) {
            inQueue.put(v.getId(), false);
        }
        queue.clear();
    }

    private void addAll(final Maximier<Variable> queue,
            final Map<Integer, Boolean> inQueue) {
        boolean change = false;

        for (Variable v : problem.getVariables()) {
            if (addInQueue(v, queue, inQueue)) {
                change = true;
            }
        }
        if (change) {
            queue.sort();
        }
    }

    private static Variable pullVariable(final Maximier<Variable> queue,
            final Map<Integer, Boolean> inQueue) {
        final Variable variable = queue.pull();
        inQueue.put(variable.getId(), false);
        return variable;
    }

    private static boolean addInQueue(final Variable variable,
            final Maximier<Variable> queue, final Map<Integer, Boolean> inQueue) {
        if (variable.getDomainSize() <= 1) {
            return false;
        }
        if (inQueue.get(variable.getId())) {
            return false;
        }
        queue.add(variable);
        inQueue.put(variable.getId(), true);
        return true;
    }

    private static void addNeighbours(final Variable variable,
            final Maximier<Variable> queue, final Map<Integer, Boolean> inQueue) {
        boolean change = false;
        for (Variable n : variable.getNeighbours()) {
            if (addInQueue(n, queue, inQueue)) {
                change = true;
            }

        }
        if (change) {
            queue.sort();
        }
    }

    public int getNbNoGoods() {
        return 0;
    }

    public int getNbSub() {
        return 0;
    }

}
