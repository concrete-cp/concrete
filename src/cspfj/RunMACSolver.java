package cspfj;

import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.util.Chronometer;

public class RunMACSolver extends AbstractRunSolver {

	private final MACSolver macSolver;

	final private static Logger logger = Logger.getLogger("cspfj.RunMACSolver");

	final private int bt;

	public RunMACSolver(final Problem problem,
			final ResultHandler resultHandler, final Heuristic heuristic,
			final boolean reverse, final Chronometer chronometer) {
		super(problem, chronometer);
		macSolver = new MACSolver(problem, resultHandler, heuristic, reverse);
		setSolver(macSolver);
		bt = problem.getMaxBacktracks();

	}

	@Override
	protected boolean launch(int factor) {

		macSolver.setMaxBacktracks(factor * bt);

		logger.info("MAC with " + macSolver.getMaxBacktracks()+" bt");
		
		try {
			
			macSolver.mac(0, null);

			return true;
		} catch (MaxBacktracksExceededException e) {

		}

		return false;
	}

}
