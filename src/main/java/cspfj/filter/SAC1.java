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

import cspfj.problem.Problem;
import cspfj.problem.IntVariable;

/**
 * @author Julien VION
 * 
 */
public final class SAC1 extends AbstractSAC {

    private final static Logger logger = Logger.getLogger(SAC1.class.getName());

    public SAC1(Problem problem) {
        super(problem, new AC3(problem));
    }

    protected boolean singletonTest(final IntVariable variable)
            throws InterruptedException {
        boolean changedGraph = false;
        for (int index = variable.getFirst(); index >= 0; index = variable
                .getNext(index)) {
            if (Thread.interrupted()) {
                throw new InterruptedException();
            }
            if (!variable.isPresent(index)) {
                continue;
            }

            // if (logger.isLoggable(Level.FINER)) {
            logger.finer(variable + " <- " + variable.getDomain().index(index)
                    + "(" + index + ")");
            // }

            problem.push();
            variable.assign(index, problem);
            nbSingletonTests++;
            final boolean consistent = filter.reduceAfter(variable);
            variable.unassign(problem);
            problem.pop();

            if (!consistent) {
                logger.fine("Removing " + variable + ", " + index);

                variable.remove(index);
                changedGraph = true;
            }
        }
        return changedGraph;
    }

    public String toString() {
        return "SAC-1 w/ " + filter;
    }
}
