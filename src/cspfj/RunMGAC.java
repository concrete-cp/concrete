package cspfj;

import java.util.Map;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;

public class RunMGAC extends AbstractRunSolver {

	private final MGAC macSolver;

	final private static Logger logger = Logger.getLogger("cspfj.RunMACSolver");

	final private int bt;

	public RunMGAC(final Problem problem,
			final ResultHandler resultHandler, final Heuristic heuristic,
			final boolean reverse, final SolutionHandler solutionHandler) {
		super(problem, solutionHandler);
		macSolver = new MGAC(problem, resultHandler, heuristic, reverse);
		setSolver(macSolver);
		bt = problem.getMaxBacktracks();
	}

	@Override
	protected boolean launch(final int factor) {

		macSolver.setMaxBacktracks(factor * bt);

		logger.info("MAC with " + macSolver.getMaxBacktracks()+" bt");
		final Map<Integer, Integer> weights = getSolutionHandler().getWeights() ;
		for (int cid: weights.keySet()) {
			getProblem().getConstraint(cid).increaseWeight(weights.get(cid)) ;
		}
		weights.clear() ;
		
		try {
			
			macSolver.mac(0, null);

			return true;
		} catch (MaxBacktracksExceededException e) {

		}

        macSolver.addNoGoods();
        getProblem().restoreAll(1);
        if (!macSolver.getFilter().reduceAll(0)) {
            return true;
        }
		return false;
	}

}
