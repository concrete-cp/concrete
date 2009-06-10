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

    public final static String TOTAL_LOAD = "total-load";
    public final static String TOTAL_SOLVE = "total-solve";
    public final static String TOTAL_NODES = "total-nodes";
    public final static String SAT = "sat";
    public final static String UNSAT = "unsat";
    public final static String UNKNOWN = "unknown";

    private Solver solver;

    private final Map<String, Object> statistics;
    private final Map<String, Object> globalStatistics;

    private static final Logger LOGGER = Logger
            .getLogger("cspfj.AbstractResultWriter");

    private int bestConflicts;

    private final boolean receiveSolutions;

    public ResultHandler() {
        this(false);
    }

    public ResultHandler(final boolean receiveSolutions) {
        this.statistics = new TreeMap<String, Object>();
        this.globalStatistics = new TreeMap<String, Object>();
        this.receiveSolutions = receiveSolutions;
        globalStatistics.put(TOTAL_LOAD, Long.valueOf(0));
        globalStatistics.put(TOTAL_SOLVE, Float.valueOf(0));
        globalStatistics.put(TOTAL_NODES, Long.valueOf(0));
        globalStatistics.put(SAT, 0);
        globalStatistics.put(UNSAT, 0);
        globalStatistics.put(UNKNOWN, 0);

    }

    public void problem(final String name) throws IOException {
        LOGGER.info("loading : " + name);
        statistics.clear();
    }

    public void load(final Solver solver, final long load) throws IOException {
        this.solver = solver;

        globalStatistics.put(TOTAL_LOAD, (Long) globalStatistics
                .get(TOTAL_LOAD)
                + load);

        LOGGER.info("loaded in " + (load / 1.0e9F) + " s");

        if (solver != null) {
            LOGGER.info(constraintStats(solver.getProblem().getConstraints()));
        }
        
        bestConflicts = Integer.MAX_VALUE;
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
            final int nbConflicts, final boolean force) throws IOException {
        if (!receiveSolutions && !force) {
            return false;
        }
        if (nbConflicts < bestConflicts) {
            bestConflicts = nbConflicts;
            LOGGER.info(solution.toString() + "(" + nbConflicts + ")");
            return true;
        }
        return false;

    }

    public void result(final Result result, final Throwable thrown)
            throws IOException {
        increment(result);

        if (solver != null) {
            globalStatistics.put(TOTAL_SOLVE, (Float) globalStatistics
                    .get(TOTAL_SOLVE)
                    + solver.getUserTime());
            globalStatistics.put(TOTAL_NODES, (Long) globalStatistics
                    .get(TOTAL_NODES)
                    + solver.getNbAssignments());
        }
        if (thrown != null) {
            final StringBuilder stb = new StringBuilder(thrown.toString());
            for (StackTraceElement e : thrown.getStackTrace()) {
                stb.append('\n').append(e);
            }
            LOGGER.warning(stb.toString());
        }

        // solver=null;

    }

    public void result(final Result result) throws IOException {
        result(result, null);
    }

    public void nextProblem() throws IOException {

    }

    public void close() throws IOException {
        LOGGER.info("Total : "
                + ((Long) globalStatistics.get(TOTAL_LOAD) / 1.0e9F)
                + " s loading and " + globalStatistics.get(TOTAL_SOLVE)
                + " s solving");
        LOGGER.info("SAT : " + globalStatistics.get(SAT) + ", UNSAT : "
                + globalStatistics.get(UNSAT) + ", UNKNOWN : "
                + globalStatistics.get(UNKNOWN));
    }

    private final void increment(final Result result) {
        switch (result) {
        case SAT:
            globalStatistics.put(SAT, (Integer) globalStatistics.get(SAT) + 1);
            break;

        case UNSAT:
            globalStatistics.put(UNSAT,
                    (Integer) globalStatistics.get(UNSAT) + 1);
            break;

        default:
            globalStatistics.put(UNKNOWN, (Integer) globalStatistics
                    .get(UNKNOWN) + 1);
        }
    }

    public final void allStatistics(final Map<String, Object> statistics) {
        LOGGER.info(statistics.toString());
        this.statistics.putAll(statistics);
    }

    public final void statistics(final String name, final Object value) {
        LOGGER.info(name + " : " + value);
        statistics.put(name, value);
    }

    public final Object getStatistic(final String name) {
        return statistics.get(name);
    }

    public final Map<String, Object> getStatistics() {
        return statistics;
    }

    public final Object getGlobalStatistic(final String name) {
        return globalStatistics.get(name);
    }

    public enum Result {
        SAT, UNSAT, UNKNOWN
    }

    public final int getBestConflicts() {
        return bestConflicts;
    }

    public final boolean isReceiveSolutions() {
        return receiveSolutions;
    }

    public final Solver getSolver() {
        return solver;
    }
}
