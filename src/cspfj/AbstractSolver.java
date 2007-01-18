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
import java.util.HashMap;
import java.util.Map;
// import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.Chronometer;
import cspfj.filter.SAC.SPACE;

public abstract class AbstractSolver implements Solver {

    protected final Problem problem;

    protected final Chronometer chronometer;

    private int nbAssignments;

    private final Map<Variable, Integer> solution;

    private int maxBacktracks;

    private int nbBacktracks;

    private int nbSolutions = 0;

    private final ResultHandler resultHandler;

    private SPACE space = SPACE.NONE;

    // private final static Logger logger =
    // Logger.getLogger("cspfj.solver.AbstractSolver") ;

    public AbstractSolver(Problem prob, ResultHandler resultHandler) {
        super();
        problem = prob;
        nbAssignments = 0;
        solution = new HashMap<Variable, Integer>();

        chronometer = new Chronometer();
        this.resultHandler = resultHandler;
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.Solver#getSolutionIndex(int)
     */
    public final int getSolutionValue(final int vId) {
        return getSolution().get(problem.getVariable(vId));
    }

    // /*
    // * (non-Javadoc)
    // *
    // * @see cspfj.Solver#getSolutionValue(int)
    // */
    // public final int getSolutionValue(final int vId) {
    // return problem.getVariable(vId).getDomain()[getSolutionIndex(vId)];
    // }

    public int getNbAssignments() {
        return nbAssignments;
    }

    protected final void incrementNbAssignments() {
        nbAssignments++;
    }

    protected final void addSolutionElement(final Variable variable,
            final int index) {
        solution.put(variable, variable.getDomain()[index]);
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.Solver#getSolution()
     */
    public final Map<Variable, Integer> getSolution() {
        return solution;
    }

    public final void setMaxBacktracks(final int maxBacktracks) {
        this.maxBacktracks = maxBacktracks;
        this.nbBacktracks = 0;
    }

    public final void checkBacktracks() throws MaxBacktracksExceededException {
        if (++nbBacktracks >= maxBacktracks && maxBacktracks >= 0) {
            throw new MaxBacktracksExceededException();
        }
    }

    public float getUserTime() {
        return chronometer.getUserTime();
    }

    protected final int getMaxBacktracks() {
        return maxBacktracks;
    }

    protected final int getNbBacktracks() {
        return nbBacktracks;
    }

    protected final void setSolution(final Map<Variable, Integer> solution) {
        this.solution.clear();
        this.solution.putAll(solution);
        nbSolutions = 1;
    }

    protected void solution(final Map<Variable, Integer> solution,
            final int nbConflicts) throws IOException {
        resultHandler.solution(solution, nbConflicts);
    }

    public final void setUseSpace(final SPACE space) {
        this.space = space;
    }

    protected final SPACE useSpace() {
        return space;
    }

    protected final void statistics(final String name, final Object value) {
        resultHandler.statistics(name, value.toString());
    }

    public final int getNbSolutions() {
        return nbSolutions;
    }

    protected final void incrementNbSolutions() {
        nbSolutions++;
    }
}
