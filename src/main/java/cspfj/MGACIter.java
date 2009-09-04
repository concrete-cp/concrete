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
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.heuristic.CrossHeuristic;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Lexico;
import cspfj.heuristic.Pair;
import cspfj.heuristic.WDegOnDom;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class MGACIter extends AbstractSolver {

    private final Filter filter;

    private final Heuristic heuristic;

    private static final Logger logger = Logger.getLogger(MGACIter.class
            .getName());

    private boolean allSolutions = false;

    private final static Problem.LearnMethod addConstraints = AbstractSolver.parameters
            .containsKey("mgac.addConstraints") ? Problem.LearnMethod
            .valueOf(AbstractSolver.parameters.get("mgac.addConstraints"))
            : Problem.LearnMethod.NONE;

    // private int level;

    public MGACIter(final Problem prob) {
        this(prob, new ResultHandler());
    }

    public MGACIter(final Problem prob, final ResultHandler resultHandler) {
        this(prob, resultHandler, new CrossHeuristic(new WDegOnDom(prob),
                new Lexico(false)));
    }

    public MGACIter(final Problem prob, final ResultHandler resultHandler,
            final Heuristic heuristic) {
        this(prob, resultHandler, heuristic, new AC3(prob));
    }

    public MGACIter(final Problem prob, final ResultHandler resultHandler,
            final Heuristic heuristic, final Filter filter) {
        super(prob, resultHandler);
        // filter = new B3C(problem, new BC(problem));
        this.filter = filter;
        this.heuristic = heuristic;

        logger.info(filter.getClass().toString());

        setMaxBacktracks(prob.getMaxBacktracks());
    }

    public boolean mac() throws MaxBacktracksExceededException, IOException {
        final Problem problem = this.problem;

        // level = 0;

        Variable selectedVariable = null;
        int selectedIndex = -1;
        do {
            // for (Variable v: problem.getVariables()) {
            // if (!v.isAssigned() && v.getDomainSize()==1) {
            // logger.fine(level + " : " + v + " <- "
            // + v.getDomain()[v.getFirst()] + "("
            // + getNbBacktracks() + "/" + getMaxBacktracks() + ")");
            // v.assign(v.getFirst(), level, problem);
            // problem.setLevelVariables(level++, v);
            // }
            // }
            if (problem.getNbFutureVariables() == 0) {
                if (getNbSolutions() < 1) {
                    for (Variable v : problem.getVariables()) {
                        addSolutionElement(v, v.getFirst());
                    }
                }

                solution();
                if (allSolutions) {
                    selectedVariable = backtrack();
                    if (selectedVariable.getDomainSize() <= 0) {
                        break;
                    }
                } else {
                    break;
                }
            }

            if (selectedVariable != null
                    && !filter.reduceAfter(selectedVariable)) {
                if (problem.getCurrentLevel() == 0) {
                    break;
                }
                selectedVariable = backtrack();
                if (selectedVariable.getDomainSize() <= 0) {
                    break;
                }
                continue;
            }

            final long pair = heuristic.selectPair(problem);

            selectedVariable = Pair.variable(pair, problem);

            assert selectedVariable.getDomainSize() > 0;

            selectedIndex = Pair.index(pair, problem);

            assert selectedVariable.isPresent(selectedIndex);

            logger.fine(problem.getCurrentLevel() + " : " + selectedVariable
                    + " <- "
                    + selectedVariable.getDomain().value(selectedIndex) + "("
                    + getNbBacktracks() + "/" + getMaxBacktracks() + ")");

            problem.setLevelVariables(selectedVariable);
            problem.push();
            selectedVariable.assign(selectedIndex, problem);
            incrementNbAssignments();

        } while (true);
        return getNbSolutions() > 0;

    }

    public Variable backtrack() throws MaxBacktracksExceededException {
        Variable selectedVariable;
        int index;
        do {
            selectedVariable = problem.getLastLevelVariable();
            index = selectedVariable.getFirst();
            selectedVariable.unassign(problem);
            problem.pop();
            problem.setLevelVariables(null);
        } while (selectedVariable.getDomainSize() <= 1
                && problem.getCurrentLevel() > 0);

        logger.finer(problem.getCurrentLevel() + " : " + selectedVariable
                + " /= " + selectedVariable.getDomain().value(index));
        selectedVariable.remove(index);
        checkBacktracks();
        return selectedVariable;
    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.Solver#run(int)
     */
    public boolean runSolver() throws IOException {
        // Logger.getLogger("").setLevel(Level.WARNING);
        // Logger.getLogger("").getHandlers()[0].setLevel(Level.WARNING);
        System.gc();
        chronometer.startChrono();

        // for (Constraint c : problem.getConstraints()) {
        // if (c instanceof ExtensionConstraint) {
        // ((ExtensionConstraint) c).getMatrixManager().countConflicts();
        // }
        // }
//        for (Constraint c : problem.getConstraints()) {
//            if (c.getArity() == 1) {
//                c.revise(new RevisionHandler() {
//                    @Override
//                    public void revised(Constraint constraint, Variable variable) {
//                        // TODO Auto-generated method stub
//                        
//                    }
//                });
//            }
//        }

        final Filter filter = getFilter();

        try {
            if (!preprocess(filter)) {
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
            try {
                if (!filter.reduceAll()) {
                    chronometer.validateChrono();
                    return false;
                }
            } catch (InterruptedException e1) {
                throw new IllegalArgumentException("Unexpected interruption");
            }
        }

        // statistics("prepro-nbskippedrevisions", Constraint
        // .getNbSkippedRevisions());

        // if (true) {
        // return false;
        // }

        final float start = chronometer.getCurrentChrono();
        heuristic.compute();

        // for (Variable v:problem.getVariables()) {
        // for (int i = v.getFirst() ; i >=0 ; i=v.getNext(i)) {
        // System.out.print(i+"-");
        // }
        // System.out.println();
        // }

        final float heuristicCpu = chronometer.getCurrentChrono();
        statistics.put("heuristic-cpu", heuristicCpu - start);

        int maxBT = allSolutions ? -1 : getMaxBacktracks();

        //
        // logger.fine("ok!") ;

        // final Random random = new Random(0);
        do {
            for (Variable v : problem.getVariables()) {
                assert v.getDomainSize() > 0;
            }
            setMaxBacktracks(maxBT);
            problem.clearLevelVariables();
            logger.info("MAC with " + maxBT + " bt");
            float macTime = -chronometer.getCurrentChrono();
            // System.out.print("run ! ");
            try {

                mac();

                break;
            } catch (MaxBacktracksExceededException e) {
                // On continue...
            } catch (OutOfMemoryError e) {
                chronometer.validateChrono();
                throw e;
            } catch (IOException e) {
                chronometer.validateChrono();
                throw e;
            }
            macTime += chronometer.getCurrentChrono();
            logger
                    .info("Took " + macTime + "s (" + (maxBT / macTime)
                            + " bps)");

            // logger.info(constraintRepartition());
            maxBT *= 1.5;
            // final Map<Variable[], List<int[]>> ngs = problem.noGoods();
            problem.noGoods(addConstraints);
            problem.reset();
            // problem.noGoodsToConstraints(ngs, addConstraints);

            try {
                if (!filter.reduceAll()) {
                    break;
                }
            } catch (InterruptedException e) {
                throw new IllegalArgumentException(
                        "Filter was unexpectingly interrupted !");
            }
        } while (true);

        final float searchCpu = chronometer.getCurrentChrono() - heuristicCpu;
        statistics.put("search-cpu", searchCpu);

        if (searchCpu > 0) {
            statistics.put("search-nps", getNbAssignments() / searchCpu);
        }

        return getNbSolutions() > 0;

    }

    public synchronized void collectStatistics() {
        chronometer.validateChrono();
        statistics.putAll(filter.getStatistics());
    }

    // private String constraintRepartition() {
    // final WeightHeuristic wvh = (WeightHeuristic) heuristic;
    // final SortedSet<Constraint> sortedConstraint = new TreeSet<Constraint>(
    // new Weight(false, wvh));
    // sortedConstraint.addAll(Arrays.asList(problem.getConstraints()));
    //
    // final StringBuilder stb = new StringBuilder();
    // final NumberFormat format = NumberFormat.getInstance();
    // format.setMaximumFractionDigits(2);
    // double total = 0;
    // for (Constraint c : sortedConstraint) {
    // stb.append(c.getName() + "(" + format.format(wvh.getWeight(c)) + ") ");
    // total += wvh.getWeight(c);
    // }
    // stb.append(" - Total = " + total);
    // return stb.toString();
    // }

    public Filter getFilter() {
        return filter;
    }

    public void setAllSolutions(final boolean allSolutions) {
        this.allSolutions = allSolutions;
    }

    public String getXMLConfig() {
        return super.getXMLConfig() + "\t\t\t<solver>" + this
                + "</solver>\n\t\t\t<filter>" + filter
                + "</filter>\n\t\t\t<heuristic>" + heuristic
                + "</heuristic>\n\t\t\t<prepro>" + getPreprocessor()
                + "</prepro>\n\t\t\t<allSolutions>" + allSolutions
                + "</allSolutions>\n";

    }

    public String toString() {
        return "maintain generalized arc consistency - iterative";
    }

}
