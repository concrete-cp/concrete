package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;

public class RunWMC extends AbstractRunSolver {

	private final WMC mCSolver;

	final private static Logger logger = Logger
			.getLogger("cspfj.RunMinConflictsSolver");

	final private int maxFlips;

	public RunWMC(final Problem problem,
			final ResultHandler resultHandler,
			final SolutionHandler solutionHandler) {
		super(problem, solutionHandler);
		mCSolver = new WMC(problem, resultHandler);
		setSolver(mCSolver);
		maxFlips = problem.getMaxFlips();

	}

	@Override
	protected boolean launch(final int factor) throws IOException {
		logger.info("MC with " + maxFlips + " flips");

		for (Constraint c : getProblem().getConstraints()) {
			c.setWeight(Math.max(1, c.getWeight()
					/ getProblem().getNbConstraints()));
		}

		mCSolver.setMaxBacktracks(maxFlips);
		try {

			mCSolver.minConflicts();

			return true;

		} catch (MaxBacktracksExceededException e) {
			getProblem().restoreAll(1);
		}

		logger.info("Max constraint weight : " + getProblem().getMaxWeight());

		for (Constraint c : getProblem().getConstraints()) {
			getSolutionHandler().addWeight(c.getId(), c.getWeight());

		}

		return false;
	}

}
