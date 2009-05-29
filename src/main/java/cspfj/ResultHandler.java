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
import java.util.Iterator;
import java.util.Map;
import java.util.TreeMap;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.problem.Variable;

public class ResultHandler {

    protected Solver solver;

    protected long totalLoad = 0;

    protected float totalSolve = 0;

    protected int sat = 0;

    protected int unsat = 0;

    protected int unknown = 0;

    protected int totalNodes = 0;

    protected final Map<String, Object> statistics;

    private static final Logger logger = Logger
            .getLogger("cspfj.AbstractResultWriter");

    private int bestSatisfied = 0;

    private final boolean receiveSolutions;

    public ResultHandler() {
        this(false);
    }

    public ResultHandler(final boolean receiveSolutions) {
        this.statistics = new TreeMap<String, Object>();
        this.receiveSolutions = receiveSolutions;
    }

    public void problem(final String name) throws IOException {
        logger.info("loading : " + name);
        statistics.clear();
        bestSatisfied = 0;
    }

    public void load(final Solver solver, final long load) throws IOException {
        this.solver = solver;

        totalLoad += load;

        logger.info("loaded in " + (load / 1.0e9F) + " s");

        logger.info(constraintStats(solver.getProblem().getConstraints()));
    }

    private String constraintStats(Constraint[] constraints) {
        final Map<String, Integer> constraintStats = new HashMap<String, Integer>();
        for (Constraint c : constraints) {
            final Integer count = constraintStats.get(c.getType());
            if (count == null) {
                constraintStats.put(c.getType(), 1);
            } else {
                constraintStats.put(c.getType(), 1 + count);
            }
        }
        final StringBuilder stb = new StringBuilder();
        final Iterator<Entry<String, Integer>> itr = constraintStats.entrySet()
                .iterator();
        if (!itr.hasNext()) {
            return "";
        }
        Entry<String, Integer> c = itr.next();
        stb.append(c.getKey()).append(": ").append(c.getValue());
        while (itr.hasNext()) {
            c = itr.next();
            stb.append('\n').append(c.getKey()).append(": ").append(
                    c.getValue());
        }
        return stb.toString();
    }

    public boolean solution(final Map<Variable, Integer> solution,
            final int nbSatisfied, final boolean force) throws IOException {
        if (!receiveSolutions && !force) {
            return false;
        }
        if (nbSatisfied > bestSatisfied) {
            bestSatisfied = nbSatisfied;
            logger.info(solution.toString() + "(" + nbSatisfied + ")");
            return true;
        }
        return false;

    }

    public void result(final Result result, final Throwable thrown)
            throws IOException {
        increment(result);

        if (solver != null) {
            totalSolve += solver.getUserTime();
            totalNodes += solver.getNbAssignments();
        }
        if (thrown != null) {
            final StringBuilder stb = new StringBuilder(thrown.toString());
            for (StackTraceElement e : thrown.getStackTrace()) {
                stb.append('\n').append(e);
            }
            logger.warning(stb.toString());
        }

        // solver=null;

    }

    public void result(final Result result) throws IOException {
        result(result, null);
    }

    public void nextProblem() throws IOException {

    }

    public void close() throws IOException {
        logger.info("Total : " + (totalLoad / 1.0e9F) + " s loading and "
                + totalSolve + " s solving");
        logger.info("SAT : " + sat + ", UNSAT : " + unsat + ", UNKNOWN : "
                + unknown);
    }

    private void increment(final Result result) {
        switch (result) {
        case SAT:
            sat++;
            break;

        case UNSAT:
            unsat++;
            break;

        default:
            unknown++;
        }
    }

    public void allStatistics(final Map<String, Object> statistics) {
        logger.info(statistics.toString());
        this.statistics.putAll(statistics);
    }

    public void statistics(final String name, final Object value) {
        logger.info(name + " : " + value);
        statistics.put(name, value);
    }

    public Object getStatistic(final String name) {
        return statistics.get(name);
    }

    public enum Result {
        SAT, UNSAT, UNKNOWN
    }

    public final int getSat() {
        return sat;
    }

    public final int getUnknown() {
        return unknown;
    }

    public final int getUnsat() {
        return unsat;
    }

    public final int getBestSatisfied() {
        return bestSatisfied;
    }

    public final boolean isReceiveSolutions() {
        return receiveSolutions;
    }

}
