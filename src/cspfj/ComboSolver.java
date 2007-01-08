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

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.exception.OutOfTimeException;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;

public class ComboSolver extends AbstractSolver {

	private final MinConflictsSolver mCSolver;

	private final MACSolver macSolver;

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.ComboSolver");

	// private final NoGoodManager noGoodManager;

	public ComboSolver(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, boolean reverse) {
		super(prob, resultHandler);

		macSolver = new MACSolver(prob, resultHandler, heuristic, reverse);
		// macSolver.enableNoGoods(2);
		// noGoodManager = macSolver.getNoGoodManager();
		mCSolver = new MinConflictsSolver(prob, resultHandler, null);

	}

	public boolean run(final long maxDuration) throws OutOfTimeException,
			IOException {
		System.gc();

		chronometer.setMaxDurationNano(maxDuration);

		long prepro = chronometer.getRemainingTimeNano();

		try {
			final Filter preprocessor;
			switch (useSpace()) {
			case BRANCH:
				preprocessor = new SAC(problem, chronometer, macSolver
						.getFilter(), true);
				break;

			case CLASSIC:
				preprocessor = new SAC(problem, chronometer, macSolver
						.getFilter(), false);
				break;

			default:
				preprocessor = macSolver.getFilter();
			}

			if (!preprocessor.reduceAll(0)) {
				chronometer.validateChrono();
				return false;
			}
		} catch (OutOfTimeException e) {
			chronometer.validateChrono();
			throw e;
		}
		checkExpiration();
		prepro -= chronometer.getRemainingTimeNano();

		switch (useSpace()) {
		case NONE:
			prepro *= problem.getNbVariables();
			break;

		default:
			prepro /= problem.getMaxDomainSize();
		}

		if (prepro < 1e8) {
			prepro = 100000000L;
		}

		logger.info("prepro : " + prepro);

		// final Random random = MinConflictsSolver.getRandom();

		long localMinimumTime = chronometer.getRemainingTimeNano();
		if (minConflicts(1, chronometer.getRemainingTime())) {
			return true;
		}
		checkExpiration();

		localMinimumTime -= chronometer.getRemainingTimeNano();

		logger.info("local : " + localMinimumTime);

		localMinimumTime *= 5;

		if (localMinimumTime < 1e8) {
			localMinimumTime = 100000000L;
		}

		boolean alt = false;

		do {
			prepro *= 1.5;

			final long macDuration;

			if (chronometer.getRemainingTimeNano() < 0) {
				macDuration = prepro;
			} else {
				macDuration = Math.min(chronometer.getRemainingTimeNano(),
						prepro);
			}

			if (mac(-1, macDuration)) {
				return getSolution().size() > 0;
			}

			checkExpiration();

			if (alt) {
				for (Constraint c : problem.getConstraints()) {
					c.setWeight(1);
				}
			}
			localMinimumTime *= 1.5;
			
			final long localDuration ;
			
			if (chronometer.getRemainingTimeNano() < 0) {
				localDuration = prepro;
			} else {
				localDuration = Math.min(chronometer.getRemainingTimeNano(),
						localMinimumTime);
			}
			
			if (minConflicts(-1, localDuration)) {
				return true;
			}

			checkExpiration();

			alt ^= true;
		} while (true);

	}

	private final boolean mac(final int maxBT, final long duration) {
		logger.info("MAC (" + duration + "s)");
		macSolver.setMaxBacktracks(maxBT);
		macSolver.setMaxDurationNano(duration);
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

	private final boolean minConflicts(final int maxLM, final long duration)
			throws IOException {
		logger.info("Local (" + duration + "s)");

		mCSolver.setMaxBacktracks(maxLM);
		mCSolver.setMaxDurationNano(duration);

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
		problem.restoreAll(1);

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
