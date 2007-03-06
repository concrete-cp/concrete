package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.problem.Problem;
import cspfj.util.Chronometer;

public class RunMCSolver extends AbstractRunSolver {

	private final MinConflictsSolver mCSolver;

	final private static Logger logger = Logger
			.getLogger("cspfj.RunMinConflictsSolver");

	final private int maxFlips;

	public RunMCSolver(final Problem problem,
			final ResultHandler resultHandler, final boolean reverse,
			final SolutionHandler solutionHandler) {
		super(problem, solutionHandler);
		mCSolver = new MinConflictsSolver(problem, resultHandler, reverse);
		setSolver(mCSolver);
		maxFlips = problem.getMaxFlips();
		
	}

	@Override
	protected boolean launch(int factor) throws IOException {
		logger.info("MC with " + factor + " x " + maxFlips + " flips");
		for (int i = factor; --i >= 0;) {
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

			logger.info("Max constraint weight : "
					+ getProblem().getMaxWeight());
		}

		for (Constraint c : getProblem().getConstraints()) {
			getSolutionHandler().addWeight(c.getId(), c.getWeight());

		}

		return false;
	}

}
