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

import java.lang.reflect.InvocationTargetException;
import java.util.Deque;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3Constraint;
import cspfj.filter.Filter;
import cspfj.generator.ProblemGenerator;
import cspfj.heuristic.CrossHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Pair;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.NoGoodLearner.LearnMethod;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspom.CSPOM;

public final class MGACIter extends AbstractSolver {

    private static final Logger LOGGER = Logger.getLogger(MGACIter.class
            .getName());

    private static final float BT_GROWTH = 1.5f;

    private static int nbAssignments;

    static {
        ParameterManager.registerObject("mgac.addConstraint", LearnMethod.BIN);
        ParameterManager.registerClass("mgac.filter", AC3Constraint.class);
        ParameterManager.registerClass("mgac.heuristic", CrossHeuristic.class);
        try {
            StatisticsManager.register(MGACIter.class
                    .getDeclaredField("nbAssignments"));
        } catch (NoSuchFieldException e) {
            throw new IllegalStateException(e);
        }
    }

    private final Deque<Pair> decisions;

    private Filter filter;

    private Heuristic heuristic;

    private final NoGoodLearner ngl;

    public MGACIter(final CSPOM cspom) throws FailedGenerationException {
        this(ProblemGenerator.generate(cspom));
    }

    public MGACIter(final Problem prob) {
        super(prob);
        setMaxBacktracks(prob.getMaxBacktracks());
        ngl = new NoGoodLearner(problem,
                (LearnMethod) ParameterManager
                        .getParameter("mgac.addConstraint"));
        decisions = new LinkedList<Pair>();
    }

    private void prepare() throws InstantiationException,
            IllegalAccessException, InvocationTargetException,
            NoSuchMethodException {
        if (filter == null) {
            filter = ((Class<Filter>) ParameterManager
                    .getParameter("mgac.filter")).getConstructor(Problem.class)
                    .newInstance(problem);
        }

        if (heuristic == null) {
            heuristic = ((Class<Heuristic>) ParameterManager
                    .getParameter("mgac.heuristic")).getConstructor(
                    Problem.class).newInstance(problem);
        }

        problem.prepare();
    }

    private boolean firstSolutionGiven = false;

    public Map<String, Integer> mac(final boolean skipFirstSolution)
            throws MaxBacktracksExceededException {
        final Problem problem = this.problem;
        boolean skipSolution = skipFirstSolution;
        Variable selectedVariable = null;
        int selectedIndex = -1;
        for (;;) {
            if (selectedVariable != null
                    && !filter.reduceAfter(selectedVariable)) {
                selectedVariable = backtrack();
                if (selectedVariable == null) {
                    break;
                }
                continue;
            }

            final Pair pair = heuristic.selectPair(problem);

            if (pair == null) {
                if (skipSolution) {
                    selectedVariable = backtrack();
                    if (selectedVariable == null) {
                        break;
                    }
                    skipSolution = false;
                    continue;
                }
                return solution();
            }
            decisions.push(pair);
            selectedVariable = pair.getVariable();

            assert selectedVariable.getDomainSize() > 0;

            selectedIndex = pair.getIndex();

            assert selectedVariable.isPresent(selectedIndex);
            //
            // LOGGER.fine(problem.getCurrentLevel() + " : " + selectedVariable
            // + " <- "
            // + selectedVariable.getDomain().value(selectedIndex) + "("
            // + getNbBacktracks() + "/" + getMaxBacktracks() + ")");

            problem.push();
            selectedVariable.setSingle(selectedIndex);
            nbAssignments++;

        }
        return null;

    }

    private Variable backtrack() throws MaxBacktracksExceededException {
        Pair decision;
        do {
            if (decisions.isEmpty()) {
                return null;
            }
            decision = decisions.pop();
            // decision.getVariable().unassign();
            problem.pop();

            // LOGGER.finer(problem.getCurrentLevel() + " : "
            // + decision.getVariable() + " /= "
            // + decision.getVariable().getValue(decision.getIndex()));
        } while (decision.getVariable().getDomainSize() <= 1);

        decision.getVariable().remove(decision.getIndex());
        checkBacktracks();
        return decision.getVariable();
    }

    public void reset() {
        firstSolutionGiven = false;
        problem.reset();
        decisions.clear();
    }

    @Override
    public Map<String, Integer> nextSolution() {
        try {
            prepare();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
        // System.gc();
        int maxBT;
        if (firstSolutionGiven) {
            maxBT = -1;
        } else {
            try {
                if (!preprocess(filter)) {
                    firstSolutionGiven = true;
                    return null;
                }
            } catch (InterruptedException e) {
                try {
                    if (!filter.reduceAll()) {
                        return null;
                    }
                } catch (InterruptedException e1) {
                    throw new IllegalStateException("Unexpected interruption");
                }
            }

            long heuristicCpu = -System.currentTimeMillis();
            heuristic.compute();

            heuristicCpu += System.currentTimeMillis();
            statistic("heuristic-cpu", heuristicCpu / 1000f);

            maxBT = getMaxBacktracks();

            // boolean entailed = false;
            // for (Iterator<Constraint> itr =
            // problem.getConstraints().iterator(); itr
            // .hasNext();) {
            // if (itr.next().isEntailed()) {
            // itr.remove();
            // entailed = true;
            // }
            // }
            // if (entailed) {
            // problem.prepare();
            // }
        }

        for (;;) {
            setMaxBacktracks(maxBT);

            LOGGER.info("MAC with " + maxBT + " bt");
            long macTime = -System.currentTimeMillis();
            int nbBT = getNbBacktracks();

            try {
                final Map<String, Integer> solution = mac(firstSolutionGiven);
                firstSolutionGiven = true;
                return solution;
            } catch (MaxBacktracksExceededException e) {
                final Set<Constraint> modified = ngl.noGoods(decisions);
                problem.reset();
                decisions.clear();

                if (!filter.reduceAfter(modified)) {
                    break;
                }

                maxBT *= BT_GROWTH;

            } finally {
                macTime += System.currentTimeMillis();
                increaseStatistic("search-cpu", macTime / 1000f);
                LOGGER.info("Took " + (macTime / 1000f) + "s ("
                        + (1000f * (getNbBacktracks() - nbBT) / macTime)
                        + " bps)");
            }

        }

        return null;

    }

    public void collectStatistics() {
        for (Entry<String, Object> e : filter.getStatistics().entrySet()) {
            statistic(e.getKey(), e.getValue());
        }
    }

    public Filter getFilter() {
        return filter;
    }

    public String getXMLConfig() {
        return super.getXMLConfig() + "\t\t\t<solver>" + this
                + "</solver>\n\t\t\t<filter>" + filter
                + "</filter>\n\t\t\t<heuristic>" + heuristic
                + "</heuristic>\n\t\t\t<prepro>" + getPreprocessor()
                + "</prepro>\n";

    }

    public String toString() {
        return "maintain generalized arc consistency - iterative";
    }

}
