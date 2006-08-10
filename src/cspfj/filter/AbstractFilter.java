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

import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Maximier;

public abstract class AbstractFilter implements Filter {

    private final boolean[] inQueue;

    private final Maximier<Variable> queue;

    protected Problem problem;

    public AbstractFilter(Problem problem) {
        super();
        this.problem = problem;
        queue = new Maximier<Variable>(new Variable[problem.getNbVariables()]);
        inQueue = new boolean[problem.getNbVariables()];

    }

    protected boolean isInQueue(final Variable variable) {
        return inQueue[variable.getId()];
    }

    protected boolean addInQueue(final Variable variable) {
        if (variable.getDomainSize() > 1 && !isInQueue(variable)) {
            queue.add(variable);
            inQueue[variable.getId()] = true;
            return true;
        }
        return false;
    }

    protected void clearQueue() {
        queue.clear();
        Arrays.fill(inQueue, false);
    }

    protected Variable pullVariable() {
        final Variable variable = queue.pull();
        inQueue[variable.getId()] = false;
        return variable;
    }

    protected void addNeighbours(final Variable variable) {
        boolean change = false;
        for (Variable n : variable.getNeighbours()) {
            change |= addInQueue(n);
        }
        if (change) {
            queue.sort();
        }
    }

    protected void addAll() {
        boolean change = false;
        for (Variable n : problem.getVariables()) {
            change |= addInQueue(n);
        }
        if (change) {
            queue.sort();
        }
    }
    
    protected boolean isQueueEmpty() {
        return queue.isEmpty() ;
    }
    
    protected int queueSize() {
        return queue.size() ;
    }
}
