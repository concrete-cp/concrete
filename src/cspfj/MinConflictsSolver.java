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
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.exception.OutOfTimeException;
import cspfj.problem.Problem;
import cspfj.problem.ProblemGenerator;
import cspfj.problem.Variable;
import cspfj.util.TieManager;
import cspfj.filter.AC3;

public final class MinConflictsSolver extends AbstractSolver {

    private final static Random random = new Random(0);

    private int nbConflicts;

    private final static Logger logger = Logger
            .getLogger("cspfj.solver.MinConflictsSolver");

    private final boolean[] toUpdate;

    private final TabuManager tabuManager;

    private final float randomWalk;

    public MinConflictsSolver(Problem prob, ResultHandler resultHandler) {
        super(prob, resultHandler);
        toUpdate = new boolean[prob.getNbVariables()];
        for (int i = 0; i < toUpdate.length; i++) {
            toUpdate[i] = false;
        }

        tabuManager = new TabuManager(problem, 15);

        randomWalk = 1F / problem.getMaxDomainSize();

    }

    public MinConflictsSolver(final ProblemGenerator generator,
            ResultHandler resultHandler) throws FailedGenerationException {
        this(Problem.load(generator), resultHandler);
    }

    private void init() {

        logger.fine("Initializing...");

        // final int[] assignments = new int[problem.getNbVariables()];

        for (Variable v : problem.getVariables()) {
            if (v.getDomainSize() < 1) {
                v.restoreLevel(1);
            }
        }

        for (Variable v : problem.getVariables()) {
            final int index = v.getBestInitialIndex(random);

            if (v.isAssigned()) {
                v.unassign(problem);
            }

            v.restoreLevel(1);

            if (logger.isLoggable(Level.FINE)) {
                logger.fine(v.toString() + " (" + v.getDomainSize() + ") <- "
                        + index);
            }
            v.assign(index, problem);
        }

        nbConflicts = weightedConflicts();

        for (Variable v : problem.getVariables()) {
            v.updateNbConflicts(random);
        }

    }

    private int realConflicts() {
        int realConflicts = 0;

        for (Constraint c : problem.getConstraints()) {
            if (!c.checkFirst()) {
                realConflicts++;
            }
        }

        return realConflicts;
    }

    private int weightedConflicts() {
        int weightedConflicts = 0;

        for (Constraint c : problem.getConstraints()) {
            if (!c.checkFirst()) {
                weightedConflicts += c.getWeight();
            }
        }

        return weightedConflicts;
    }

    public void minConflicts() throws MaxBacktracksExceededException,
            OutOfTimeException, IOException {

        init();

        logger.fine("Searching...");

        int bestEver = Integer.MAX_VALUE;

        final TieManager tieManager = new TieManager();

        while (nbConflicts > 0) {
            if (nbConflicts < bestEver) {
                bestEver = nbConflicts;
                final Map<Variable, Integer> solution = new HashMap<Variable, Integer>();
                for (Variable v : problem.getVariables()) {
                    solution.put(v, v.getDomain()[v.getFirstPresentIndex()]);
                }
                solution(solution, nbConflicts);
            }

            if (logger.isLoggable(Level.FINE)) {
                final int realConflicts = realConflicts();

                logger.fine(nbConflicts + " conflicts " + "(real = "
                        + realConflicts + ", " + getNbBacktracks() + "/"
                        + getMaxBacktracks() + ")");

            }
            assert realConflicts() <= nbConflicts;

            try {
                chronometer.checkExpiration();
            } catch (OutOfTimeException e) {
                if (getNbBacktracks() >= 1) {
                    throw e;
                }
            }

            if (random.nextFloat() < randomWalk) {
                // checkBacktracks();

                final Variable variable = problem.getVariable(random
                        .nextInt(problem.getNbVariables()));

                final int oldIndex = variable.getFirstPresentIndex();

                variable.unassign(problem);

                final int index = variable.assignRandomPresentIndex(random,
                        problem);

                if (oldIndex != index) {
                    nbConflicts += variable.getImprovment(index);

                    variable.updateNbConflicts(random);
                    for (Variable n : variable.getNeighbours()) {
                        n.updateNbConflicts(random);
                    }
                }
                assert nbConflicts == weightedConflicts() : nbConflicts + "/="
                        + weightedConflicts();

            } else {

                Variable bestVariable = null;

                tieManager.clear();

                int bestImp = tieManager.getBestEvaluation();

                for (Variable v : problem.getVariables()) {

                    if (v.getNbConflicts() <= -bestImp) {
                        continue;
                    }

                    final int index = v.bestImprovment(tabuManager, random,
                            bestEver - nbConflicts);
                    if (index < 0) {
                        continue;
                    }

                    if (tieManager.newValue(index, v.getImprovment(index),
                            random)) {
                        bestVariable = v;
                        bestImp = tieManager.getBestEvaluation();
                    }

                }

                if (tieManager.getBestValue() < 0) {
                    logger.fine("Cleaning tabu list");
                    tabuManager.clean();
                    checkBacktracks();
                    continue;
                }

                tabuManager.push(bestVariable, tieManager.getBestValue());

                bestVariable.unassign(problem);
                bestVariable.assign(tieManager.getBestValue(), problem);

                nbConflicts += bestImp;

                if (bestImp >= 0) {
                    // Minimum local
                    checkBacktracks();
                    for (Constraint c : problem.getConstraints()) {
                        if (!c.checkFirst()) {
                            nbConflicts++;
                            c.increaseWeight();
                            for (Variable v : c.getInvolvedVariables()) {
                                toUpdate[v.getId()] = true;
                            }
                        }
                    }

                }

                toUpdate[bestVariable.getId()] = true;
                for (Variable n : bestVariable.getNeighbours()) {
                    toUpdate[n.getId()] = true;
                }

                for (int i = 0; i < problem.getNbVariables(); i++) {
                    if (toUpdate[i]) {
                        problem.getVariable(i).updateNbConflicts(random);
                        toUpdate[i] = false;
                    }
                }

                assert nbConflicts == weightedConflicts() : nbConflicts + "/="
                        + weightedConflicts();

            }

            incrementNbAssignments();

        }

        for (Variable v : problem.getVariables()) {
            addSolutionElement(v, v.getFirstPresentIndex());
        }

        assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
                + " conflicts ! (" + weightedConflicts() + " wc)";

    }

    public boolean run(final int maxDuration) throws OutOfTimeException,
            IOException {
        int localBT = (int) Math.sqrt(problem.getNbVariables()
                * problem.getMaxDomainSize());
        boolean resolved = false;
        System.gc();

        setMaxDuration(maxDuration);

        if (!new AC3(problem).reduceAll(0)) {
            chronometer.validateChrono();
            return false;
        }

        do {
            setMaxBacktracks(localBT);
            try {
                minConflicts();
                resolved = true;
            } catch (MaxBacktracksExceededException e) {
                problem.restoreAll();
            } catch (OutOfTimeException e) {
                chronometer.validateChrono();
                throw e;
            } catch (OutOfMemoryError e) {
                chronometer.validateChrono();
                throw e;
            } catch (IOException e) {
                chronometer.validateChrono();
                throw e;
            }

            for (Constraint c : problem.getConstraints()) {
                c.setWeight(Math.max(1, (int) Math.sqrt(c.getWeight())));
            }
            localBT *= 1.2;
        } while (!resolved);

        chronometer.validateChrono();
        return true;

    }

    public static Random getRandom() {
        return random;
    }
}
