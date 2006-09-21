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
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.problem.Problem;
import cspfj.problem.ProblemGenerator;

public class ComboSolver2 extends AbstractSolver {

	private final MinConflictsSolver mCSolver;

	private final ResultHandler resultHandler;

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.ComboSolver");

	// private final NoGoodManager noGoodManager;

	public ComboSolver2(Problem prob, ResultHandler resultHandler) {
		super(prob, resultHandler);

		this.resultHandler = resultHandler;

		// macSolver.enableNoGoods(2);
		// noGoodManager = macSolver.getNoGoodManager();
		mCSolver = new MinConflictsSolver(prob, resultHandler);

	}

	public ComboSolver2(final ProblemGenerator generator,
			ResultHandler resultHandler) throws FailedGenerationException {
		this(Problem.load(generator), resultHandler);
	}

	public boolean run(final int maxDuration) throws OutOfTimeException,
			IOException {
		System.gc();

		chronometer.setMaxDuration(maxDuration);

		try {
			final Filter preprocessor;
			switch (useSpace()) {
			case BRANCH:
				preprocessor = new SAC(problem, chronometer, new MACSolver(
						problem, resultHandler).getFilter(), true);
				break;

			case CLASSIC:
				preprocessor = new SAC(problem, chronometer, new MACSolver(
						problem, resultHandler).getFilter(), false);
				break;

			default:
				preprocessor = new MACSolver(problem, resultHandler)
						.getFilter();
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

		// boolean alt = false;

		int nbFlips = (int) Math.sqrt(problem.getNbVariables()) ;
		int additionalConstraints = 0;
		do {
			nbFlips *= 1.1;
//			for (Constraint c : problem.getConstraints()) {
//				c.setWeight(1);
//			}
			problem.updateInvolvingConstraints();
			if (minConflicts(nbFlips, chronometer.getRemainingTime())) {
				return true;
			}

			checkExpiration();

			//int activeConstraints = 0;
			for (Constraint c : problem.getConstraints()) {

				c.setActive(false);

			}

			problem.restoreAll(1);

			final Problem coreProblem = Problem.activeProblem(problem,
					(int) (problem.getNbConstraints() * (1 - Math.exp(-.05
							* additionalConstraints++))));

			logger.info(coreProblem.getNbConstraints() + " constraints, "
					+ coreProblem.getNbVariables() + " variables");

			if (!mac(coreProblem, chronometer.getRemainingTime())) {
				return false;
			}
			
			coreProblem.addNoGoods();
			
			coreProblem.restoreAll(1);

		} while (true);

	}

	private final boolean mac(final Problem problem, final int duration)
			throws OutOfTimeException {
		// logger.fine("Initializing value orders");
		// problem.setValueOrders(random);

		logger.info("MAC (" + duration + "s)");
		final MACSolver macSolver = new MACSolver(problem, resultHandler);
		try {
			final boolean result = macSolver.run(duration);
			if (result) {
				setSolution(macSolver.getSolution());
			}
			chronometer.validateChrono();
			return result;
		} catch (OutOfTimeException e) {
			chronometer.validateChrono();
			throw e;
		} catch (OutOfMemoryError e) {
			chronometer.validateChrono();
			throw e;
		}

		// final int ng = macSolver.addNoGoods();
		//
		// logger.info(ng + " noGoods");

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

		return false;
	}

	@Override
	public int getNbAssignments() {
		// TODO: manque MAC
		return mCSolver.getNbAssignments();
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
