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
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.CrossHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Lexico;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.NoGoodLearner;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.problem.NoGoodLearner.LearnMethod;

public final class MGACIter extends AbstractSolver {

    private static final Logger LOGGER = Logger.getLogger(MGACIter.class
            .getName());

    private static final float BT_GROWTH = 1.5f;
    private static final LearnMethod LEARN_METHOD;

    static {
        final String lm = AbstractSolver.PARAMETERS.get("mgac.addConstraints");
        if (lm == null) {
            LEARN_METHOD = LearnMethod.NONE;
        } else {
            LEARN_METHOD = LearnMethod.valueOf(lm);
        }
    }
    private final Filter filter;

    private final Heuristic heuristic;

    public MGACIter(final Problem prob) {
        this(prob, new CrossHeuristic(new WDegOnDom(prob), new Lexico(false)));
    }

    public MGACIter(final Problem prob, final Heuristic heuristic) {
        this(prob, heuristic, new AC3(prob));
    }

    public MGACIter(final Problem prob, final Heuristic heuristic,
            final Filter filter) {
        super(prob);
        // filter = new B3C(problem, new BC(problem));
        this.filter = filter;
        this.heuristic = heuristic;

        LOGGER.info(filter.getClass().toString());

        setMaxBacktracks(prob.getMaxBacktracks());
    }

    public Map<Variable, Integer> mac() throws MaxBacktracksExceededException,
            IOException {
        final Problem problem = this.problem;

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
                return solution();
            }

            selectedVariable = pair.getVariable();

            assert selectedVariable.getDomainSize() > 0;

            selectedIndex = pair.getIndex();

            assert selectedVariable.isPresent(selectedIndex);

            LOGGER.fine(problem.getCurrentLevel() + " : " + selectedVariable
                    + " <- "
                    + selectedVariable.getDomain().value(selectedIndex) + "("
                    + getNbBacktracks() + "/" + getMaxBacktracks() + ")");

            problem.setCurrentLevelVariable(selectedVariable);
            problem.push();
            selectedVariable.assign(selectedIndex);
            incrementNbAssignments();

        }
        return null;

    }

    public Variable backtrack() throws MaxBacktracksExceededException {
        Variable selectedVariable;
        int index;
        do {
            selectedVariable = problem.getLastLevelVariable();
            if (selectedVariable == null) {
                return null;
            }
            index = selectedVariable.getFirst();
            selectedVariable.unassign();
            problem.pop();
            problem.setCurrentLevelVariable(null);
        } while (selectedVariable.getDomainSize() <= 1);

        LOGGER.finer(problem.getCurrentLevel() + " : " + selectedVariable
                + " /= " + selectedVariable.getDomain().value(index));
        selectedVariable.remove(index);
        checkBacktracks();
        return selectedVariable;
    }

    @Override
    public Map<Variable, Integer> solve() throws IOException {
        System.gc();
        startChrono();

        try {
            if (!preprocess(filter)) {
                validateChrono();
                return null;
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
            try {
                if (!filter.reduceAll()) {
                    validateChrono();
                    return null;
                }
            } catch (InterruptedException e1) {
                throw new IllegalArgumentException("Unexpected interruption");
            }
        }

        final float start = getCurrentChrono();
        heuristic.compute();

        final float heuristicCpu = getCurrentChrono();
        statistic("heuristic-cpu", heuristicCpu - start);

        int maxBT = getMaxBacktracks();

        Map<Variable, Integer> solution = null;
        for (;;) {
            for (Variable v : problem.getVariables()) {
                assert v.getDomainSize() > 0;
            }
            setMaxBacktracks(maxBT);
            problem.clearLevelVariables();
            LOGGER.info("MAC with " + maxBT + " bt");
            float macTime = -getCurrentChrono();
            // System.out.print("run ! ");
            try {

                solution = mac();

                break;
            } catch (MaxBacktracksExceededException e) {
                // On continue...
            } catch (OutOfMemoryError e) {
                validateChrono();
                throw e;
            } catch (IOException e) {
                validateChrono();
                throw e;
            }
            macTime += getCurrentChrono();
            LOGGER.info("Took " + macTime + "s (" + (maxBT / macTime) + " bps)");

            maxBT *= BT_GROWTH;

            NoGoodLearner.noGoods(problem, LEARN_METHOD);
            problem.reset();

            try {
                if (!filter.reduceAll()) {
                    break;
                }
            } catch (InterruptedException e) {
                throw new IllegalArgumentException(
                        "Filter was unexpectingly interrupted !");
            }
        }

        final float searchCpu = getCurrentChrono() - heuristicCpu;
        statistic("search-cpu", searchCpu);

        if (searchCpu > 0) {
            statistic("search-nps", getNbAssignments() / searchCpu);
        }

        return solution;

    }

    public void collectStatistics() {
        validateChrono();
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
