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

import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.exception.OutOfTimeException;
import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.heuristic.Pair;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public final class MACSolver extends AbstractSolver {

    private final Filter filter;

    private final Heuristic heuristic;

    private static final Logger logger = Logger.getLogger("cspfj.MACSolver");

    // private NoGoodManager noGoodManager = null;

    // private int maxNoGoodSize;

    public MACSolver(Problem prob, ResultHandler resultHandler, Heuristic heuristic) {
        super(prob, resultHandler);
        filter = new AC3(problem);
        // heuristic = new WDegOnDomBySupports(prob);
        this.heuristic = heuristic ;
        setMaxBacktracks(problem.getNbVariables());
    }

    // public void enableNoGoods(final int maxSize) {
    // // noGoodManager = new NoGoodManager(problem.getNbVariables(), 2);
    // maxNoGoodSize = maxSize;
    // }

    public boolean mac(final int level, final Variable lastModifiedVariable)
            throws MaxBacktracksExceededException, OutOfTimeException {
        if (problem.getNbFutureVariables() == 0) {

            return true;
        }

        chronometer.checkExpiration();

        if (!filter.reduceAfter(level, lastModifiedVariable)) {
            return false;
        }

        final Pair pair = heuristic.selectPair();

        final Variable selectedVariable = pair.getVariable();

        final int selectedIndex = pair.getValue();

        // final int selectedIndex = selectedVariable.getFirstPresentIndex();

        final int domainSizeBefore = selectedVariable.getDomainSize();

        if (logger.isLoggable(Level.FINE)) {
            logger.fine(level + " : " + selectedVariable + " <- "
                    + selectedVariable.getDomain()[selectedIndex] + "("
                    + getNbBacktracks() + "/" + getMaxBacktracks() + ")");
        }

        selectedVariable.assign(selectedIndex, problem);

        problem.setLevelVariables(level, selectedVariable.getId());

        incrementNbAssignments();

        // removeNoGoods(level);

        if (mac(level + 1, domainSizeBefore > 1 ? selectedVariable : null)) {
            addSolutionElement(selectedVariable, selectedIndex);
            return true;
        }

        checkBacktracks();

        // problem.increaseWeights();
        selectedVariable.unassign(problem);
        problem.restore(level + 1);

        problem.setLevelVariables(level, -1);

        if (selectedVariable.getDomainSize() <= 1) {
            return false;
        }
        // System.out.println(level + " : " + selectedVariable + " /= "
        // + selectedVariable.getDomain()[selectedIndex]);
        selectedVariable.remove(selectedIndex, level);

        return mac(level, selectedVariable);

    }

    /*
     * (non-Javadoc)
     * 
     * @see cspfj.Solver#run(int)
     */
    public boolean run(final int maxDuration) throws OutOfTimeException {
        int maxBT = getMaxBacktracks();
        boolean result;

        System.gc();
        // enableNoGoods(2);
        setMaxDuration(maxDuration);

        try {
            final Filter preprocessor;
            switch (useSpace()) {
            case BRANCH:
                preprocessor = new SAC(problem, chronometer, getFilter(), true);
                break;

            case CLASSIC:
                preprocessor = new SAC(problem, chronometer, getFilter(), false);
                break;

            default:

                preprocessor = getFilter();
            }

            final float start = chronometer.getCurrentChrono();
            if (!preprocessor.reduceAll(0)) {
                chronometer.validateChrono();
                return false;
            }

            int removed = 0;

            for (Variable v : problem.getVariables()) {
                removed += v.getDomain().length - v.getDomainSize();
            }
            
            statistics("prepro-removed", removed) ;
//            statistics("prepro-subs", preprocessor.getNbSub()) ;
            statistics("prepro-nogoods", preprocessor.getNbNoGoods());
            statistics("prepro-cpu", chronometer.getCurrentChrono() - start);
            statistics("prepro-ccks", Constraint.getNbChecks());
            statistics("prepro-nbpresencechecks", Constraint.getNbPresenceChecks());
            
            
            statistics("prepro-nbeffectiverevisions", Constraint.getNbEffectiveRevisions());
            statistics("prepro-nbuselessrevisions", Constraint.getNbUselessRevisions());
            statistics("prepro-nbskippedrevisions", Constraint.getNbSkippedRevisions());
            
            
            if (preprocessor instanceof SAC) {
            	statistics("prepro-singletontests", ((SAC)preprocessor).getNbSingletonTests());
            }

        } catch (OutOfTimeException e) {
            chronometer.validateChrono();
            throw e;
        }

        
        heuristic.compute() ;
        //
        // logger.fine("ok!") ;

        // final Random random = new Random(0);
        do {
            // System.out.print("run ! ");
            try {
                setMaxBacktracks(maxBT);
                result = mac(0, null);
                break;
            } catch (MaxBacktracksExceededException e) {
                // On continue...
            } catch (OutOfTimeException e) {
                chronometer.validateChrono();
                throw e;
            } catch (OutOfMemoryError e) {
                chronometer.validateChrono();
                throw e;
            }

            maxBT *= 1.5;
            addNoGoods();
            problem.restoreAll(1);
        } while (true);

        chronometer.validateChrono();
        return result;

    }

    public int addNoGoods() {
        return problem.addNoGoods();
    }

    //
    // private void removeNoGoods(final int level) {
    //
    // logger.info("removing noGoods");
    //
    // final Variable scope[] = new Variable[level + 1];
    // final int[] tuple = new int[level + 1];
    //
    // for (int l = 0; l <= level; l++) {
    // scope[l] = problem.getVariable(levelVariables[l]);
    // tuple[l] = problem.getVariable(levelVariables[l])
    // .getFirstPresentIndex();
    // }
    //
    // for (int[] couple : noGoodManager.removable(scope, tuple)) {
    // if (problem.getVariable(couple[0]).isPresent(couple[1])) {
    // problem.getVariable(couple[0]).remove(couple[1], level);
    // logger.info(couple[0] + " /= " + couple[1]);
    // }
    // }
    //
    // } // private void removeNoGoods(final int level) { // final Variable

    // scope[] = new Variable[level + 1];
    // final int[] tuple = new int[level + 1];
    //
    // for (int l = 0; l <= level; l++) {
    // scope[l] = problem.getVariable(levelVariables[l]);
    // tuple[l] = problem.getVariable(levelVariables[l])
    // .getFirstPresentIndex();
    // }
    //
    // final int[] noGood = noGoodManager.createNoGood(scope, tuple);
    //
    // for (Variable v : problem.getVariables()) {
    // if (noGood[v.getId()] >= 0) {
    // continue;
    // }
    //
    // for (int i = v.getFirstPresentIndex(); i < v.getDomain().length; i++) {
    // if (!v.isPresent(i)) {
    // continue;
    // }
    //
    // noGood[v.getId()] = i;
    //
    // if (noGoodManager.isNoGood(noGood)) {
    // v.remove(i, level);
    // }
    // }
    //
    // noGood[v.getId()] = -1;
    // }
    //
    // }

    // public NoGoodManager getNoGoodManager() {
    // return noGoodManager;
    // }

    public Filter getFilter() {
        return filter;
    }

    // public int getMaxNoGoodSize() {
    // return maxNoGoodSize;
    // }

}
