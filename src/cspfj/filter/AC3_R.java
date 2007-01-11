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

/**
 * @author scand1sk
 * 
 */
public final class AC3_R implements Filter {

    private static final Logger logger = Logger.getLogger("cspfj.AC3");

    private final Problem problem;

    private final ArcQueue[] queue;

    public AC3_R(final Problem problem) {
        super();
        this.problem = problem;

        queue = new ArcQueue[problem.getMaxVId() + 1];

        for (Variable v : problem.getVariables()) {
            queue[v.getId()] = new ArcQueue(v.getInvolvingConstraints().length);
        }
    }

    public boolean reduceAll(final int level) {
        clearQueue();
        final ArcQueue[] queue = this.queue;
        for (Variable v : problem.getVariables()) {
            if (!v.isAssigned()) {
                queue[v.getId()].fill();
            }
        }
        return reduce(level);
    }

    public boolean reduceAfter(final int level, final Variable variable) {
        if (variable == null) {
            return true;
        }

        clearQueue();

        for (Constraint c : variable.getInvolvingConstraints()) {
            addNeighbours(variable, c);
        }

        return reduce(level);
    }

    private boolean reduce(final int level) {
        final ArcQueue[] queue = this.queue;

        while (true) {
            final Variable variable = pullVariable();

            if (variable == null) {
                assert checkAC();
                return true;
            }

            final int vId = variable.getId();

            final Constraint[] involvingConstraints = variable
                    .getInvolvingConstraints();

            int effectiveRevisions = 0;

            int lastRevisedConstraint = -1;

            final ArcQueue arcs = queue[vId];

            for (int c = arcs.getSize(); --c >= 0;) {
                final int cId = arcs.getArc(c);

                final Constraint constraint = involvingConstraints[cId];

                if (constraint.revise(variable, level)) {
                    if (variable.getDomainSize() <= 0) {
                        constraint.increaseWeight();
                        return false;
                    }
                    lastRevisedConstraint = cId;
                    effectiveRevisions++;
                }
            }
            arcs.clear();

            if (effectiveRevisions == 1) {
                for (int c = involvingConstraints.length; --c >= 0;) {
                    if (c != lastRevisedConstraint) {
                        addNeighbours(variable, involvingConstraints[c]);
                    }
                }
            } else if (effectiveRevisions > 1) {
                for (int c = involvingConstraints.length; --c >= 0;) {
                    addNeighbours(variable, involvingConstraints[c]);
                }
            }

        }
    }

    private void clearQueue() {
        for (ArcQueue a : queue) {
            a.clear();
        }
    }

    private Variable pullVariable() {
        Variable bestVariable = null;
        int bestValue = Integer.MAX_VALUE;

        final Problem problem = this.problem;

        final ArcQueue[] queue = this.queue;

        for (int i = queue.length; --i >= 0;) {
            if (queue[i].hasArcs()) {
                final Variable variable = problem.getVariable(i);
                final int domainSize = variable.getDomainSize();
                if (domainSize < bestValue) {
                    bestVariable = variable;
                    bestValue = domainSize;
                }
            }
        }

        return bestVariable;
    }

    // private void addInQueue(final int vId) {
    // // if (!inQueue[vId]) {
    // inQueue[vId] = true;
    // // queueSize++;
    // // }
    // }

    // private void addInQueue(final int vId) {
    // // if (!inQueue[vId]) {
    // inQueue[vId] = true;
    // // varChain.add(vId);
    // // found = true ;
    // // }
    // }

    private void addNeighbours(Variable variable, Constraint involvingConstraint) {
        final ArcQueue[] queue = this.queue;
        for (Variable neighbour : involvingConstraint.getInvolvedVariables()) {
            if (neighbour != variable && !neighbour.isAssigned()) {
                queue[neighbour.getId()].add(involvingConstraint
                        .getPositionInVariable(neighbour));
            }
        }
    }

    public int getNbNoGoods() {
        return 0;
    }

    private boolean checkAC() {
        for (Variable v : problem.getVariables()) {
            for (int index = v.getFirst(); index >= 0; index = v.getNext(index)) {
                for (Constraint c : v.getInvolvingConstraints()) {
                    if (!c.findValidTuple(v, index)) {
                        logger.severe("Could not find support for " + v + ", "
                                + v.getDomain()[index] + " in " + c);
                        logger.severe(problem.toString());
                        return false;
                    }
                    // problem;
                    // assert c.check() : c + "is not valid !";
                }
            }
        }
        return true;
    }

    public String toString() {
    	return "AC3rm-reverse" ;
    }
}
