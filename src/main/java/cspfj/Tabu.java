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
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.LSVariable;
import cspfj.problem.Problem;
import cspfj.problem.Variable;
import cspfj.util.TieManager;

public final class Tabu extends AbstractLocalSolver {

    private final static Logger logger = Logger.getLogger("cspfj.Tabu");

    private final TabuManager tabuManager;

    private final TieManager tieManager;

    private int bestEver = Integer.MAX_VALUE;

    public Tabu(Problem prob, ResultHandler resultHandler, boolean max) {
        this(prob, resultHandler, -1, max);
    }

    public Tabu(Problem prob, ResultHandler resultHandler, int tabuSize,
            boolean max) {
        super(prob, resultHandler, max);
        tabuManager = new TabuManager(prob, tabuSize < 0 ? 30 : tabuSize);
        tieManager = new TieManager(getRandom());
    }

    private int bestWalk(final int aspiration) {
        LSVariable bestVariable = null;
        final TieManager tieManager = this.tieManager;
        tieManager.clear();

        // int bestImp = tieManager.getBestEvaluation();
        final int nbIt = getNbBacktracks();
        for (LSVariable vcm : lsVariables.values()) {

            // if (!vcm.isCritic()) {
            // continue;
            // }

            // assert vcm.getCurrentConflicts() > 0 : vcm.getVariable() + " : "
            // + vcm.getCurrentConflicts()
            // + Arrays.toString(vcm.criticConstraints);

            final Variable variable = vcm.getVariable();

            final int vId = variable.getId();
            for (int i = variable.getFirst(); i >= 0; i = variable.getNext(i)) {
                if ((!tabuManager.isTabu(vId, i, nbIt) || vcm.getImprovment(i) < -aspiration)
                        && tieManager.newValue(i, vcm.getImprovment(i))) {
                    bestVariable = vcm;
                }
            }
        }

        if (bestVariable == null) {
            return 0;
        }

        final int bestIndex = tieManager.getBestValue();

        final int bestImp = bestVariable.getImprovment(bestIndex);

        tabuManager.push(bestVariable.getVariable().getId(), bestVariable
                .getAssignedIndex(), getNbBacktracks());

        // if (FINER) {
        logger.finer(bestVariable + " <- " + bestIndex);
        // }
        bestVariable.reAssign(bestIndex);

        return bestImp;
    }

    public void minConflicts() throws MaxBacktracksExceededException,
            IOException {

        //final Problem problem = this.problem;
        // final float randomWalk = this.rWProba;

        init();
        tabuManager.clean();
        int nbConflicts = conflicts();

        logger.fine("Searching...");

        while (nbConflicts > 0) {
            if (nbConflicts < bestEver) {
                bestEver = nbConflicts;
                solution(nbConflicts);
            }

            // if (FINER) {
            logger.finer(nbConflicts + " conflicts " + "(real = "
                    + realConflicts() + ", " + getNbBacktracks() + "/"
                    + getMaxBacktracks() + ")");
            // }

            assert realConflicts() <= nbConflicts;

            // if (random.nextFloat() < randomWalk) {
            // nbConflicts += randomWalk();
            // } else {
            nbConflicts += bestWalk(nbConflicts - bestEver);
            // }

            assert nbConflicts == conflicts() : nbConflicts + "/="
                    + conflicts() + " (real = " + realConflicts() + ")";
            incrementNbAssignments();
            checkBacktracks();

        }

        solution(0);
        assert realConflicts() == 0 : getSolution() + " -> " + realConflicts()
                + " conflicts ! (" + conflicts() + " wc)";

    }

    public String toString() {
        return "tabu min-conflicts - " + tabuManager.getSize();
    }

}
