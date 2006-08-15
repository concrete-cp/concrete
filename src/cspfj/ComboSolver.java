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
import java.util.Random;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.FailedGenerationException;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.exception.OutOfTimeException;
import cspfj.filter.SAC;
import cspfj.problem.Problem;
import cspfj.problem.ProblemGenerator;

public class ComboSolver extends AbstractSolver {

    private final MinConflictsSolver mCSolver;

    private final MACSolver macSolver;

    private final static Logger logger = Logger
            .getLogger("cspfj.solver.ComboSolver");

    // private final NoGoodManager noGoodManager;

    public ComboSolver(Problem prob, ResultHandler resultHandler) {
        super(prob, resultHandler);

        macSolver = new MACSolver(prob, resultHandler);
        macSolver.enableNoGoods(2);
        // noGoodManager = macSolver.getNoGoodManager();
        mCSolver = new MinConflictsSolver(prob, resultHandler);

    }

    public ComboSolver(final ProblemGenerator generator,
            ResultHandler resultHandler) throws FailedGenerationException {
        this(Problem.load(generator), resultHandler);
    }

    public boolean run(final int maxDuration) throws OutOfTimeException,
            IOException {
        System.gc();

        chronometer.setMaxDuration(maxDuration);

        long prepro = chronometer.getRemainingTimeNano();

        try {
            if (!new SAC(problem, macSolver.getMaxNoGoodSize(), chronometer, macSolver.getFilter())
                    .reduceAll(0)) {
//            if (!macSolver.getFilter().reduceAll(0)) {
                chronometer.validateChrono();
                return false;
            }
        } catch (OutOfTimeException e) {
            chronometer.validateChrono();
            throw e;
        }
        checkExpiration();
        prepro -= chronometer.getRemainingTimeNano();
        prepro /= problem.getMaxDomainSize();
//        prepro *=Math.sqrt(problem.getNbVariables());
        if (prepro < 1e9) {
            prepro = 1000000000L;
        }


        final Random random = MinConflictsSolver.getRandom();

        long localMinimumTime = chronometer.getRemainingTimeNano();
        if (minConflicts(1, chronometer.getRemainingTime())) {
            return true;
        }
        checkExpiration();

        localMinimumTime -= chronometer.getRemainingTimeNano();
        localMinimumTime *= 5;
        if (localMinimumTime < 1e9) {
            localMinimumTime = 1000000000L;
        }

        

        boolean alt = false;

        do {
            prepro *= 1.5;

            if (mac(-1, Math.min(chronometer.getRemainingTime(), Math
                    .round(prepro / 1e9f)), random)) {
                return getSolution().size() > 0;
            }

            checkExpiration();

            if (alt) {
                for (Constraint c : problem.getConstraints()) {
                    c.setWeight(1);
                }
            }
            localMinimumTime *= 1.5;
            if (minConflicts(-1, Math.min(chronometer.getRemainingTime(), Math
                    .round(localMinimumTime / 1e9f)))) {
                return true;
            }

            checkExpiration();

            alt ^= true;
        } while (true);

    }

    private final boolean mac(final int maxBT, final int duration,
            final Random random) {
        logger.fine("Initializing value orders");
        problem.setValueOrders(random);

        logger.info("MAC (" + duration + "s)");
        macSolver.setMaxBacktracks(maxBT);
        macSolver.setMaxDuration(duration);
        try {
            if (macSolver.mac(0, null)) {
                // logger.info(macSolver.getSolution().toString());
                setSolution(macSolver.getSolution());
            }
            chronometer.validateChrono();
            return true;
        } catch (MaxBacktracksExceededException e) {
            // Continue
        } catch (OutOfTimeException e) {
            // Continue
        } catch (OutOfMemoryError e) {
            chronometer.validateChrono();
            throw e;
        }

        final int ng = macSolver.addNoGoods();

        logger.info(ng + " noGoods");

        return false;
    }

    private final boolean minConflicts(final int maxLM, final int duration)
            throws IOException {
        logger.info("Local (" + duration + "s)");

        mCSolver.setMaxBacktracks(maxLM);
        mCSolver.setMaxDuration(duration);

        try {

            mCSolver.minConflicts();

            setSolution(mCSolver.getSolution());
            chronometer.validateChrono();
            return true;
        } catch (MaxBacktracksExceededException e) {

        } catch (OutOfTimeException e) {

        } catch (OutOfMemoryError e) {
            chronometer.validateChrono();
            throw e;
        } catch (IOException e) {
            chronometer.validateChrono();
            throw e;
        }
        problem.restoreAll();

        return false;
    }

    @Override
    public int getNbAssignments() {
        return macSolver.getNbAssignments() + mCSolver.getNbAssignments();
    }

    public void checkExpiration() throws OutOfTimeException {
        try {
            chronometer.checkExpiration();
        } catch (OutOfTimeException e) {
            chronometer.validateChrono();
            throw e;
        }
    }

}
