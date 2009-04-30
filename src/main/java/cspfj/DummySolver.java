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

package cspfj;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.security.InvalidParameterException;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.DiscHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Lexico;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class DummySolver extends AbstractSolver {

    public DummySolver(final Problem prob, final ResultHandler resultHandler) {
        super(prob, resultHandler);
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.Solver#run(int)
     */
    public boolean runSolver() throws IOException {
        System.gc();
        chronometer.startChrono();
        try {
            if (!preprocess(new AC3(problem))) {
                chronometer.validateChrono();
                return false;
            }
        } catch (InstantiationException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (IllegalAccessException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (InvocationTargetException e1) {
            throw new IllegalStateException(e1);
        } catch (NoSuchMethodException e1) {
            throw new InvalidParameterException(e1.toString());
        } catch (InterruptedException e) {
            throw new IllegalArgumentException("Unexpected interruption");
        }
        chronometer.validateChrono();
        for (Variable v : problem.getVariables()) {
            addSolutionElement(v, 0);
        }
        return true;
    }

    public synchronized void collectStatistics() {
        chronometer.validateChrono();
    }

    public String getXMLConfig() {
        return "\t\t\t<solver>" + this + "</solver>\n\t\t\t<prepro>"
                + getPreprocessor() + "</prepro>\n";
    }

    public String toString() {
        return "dummy";
    }

}
