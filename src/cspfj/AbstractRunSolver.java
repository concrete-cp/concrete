package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.problem.Problem;
import cspfj.util.Chronometer;

public abstract class AbstractRunSolver extends Thread {

	final private Problem problem;

	private Solver solver;

	final private static Logger logger = Logger
			.getLogger("cspfj.RunSolverAbstract");

	final private Chronometer chronometer;

	final private SolutionHandler solutionHandler ;
	


	protected AbstractRunSolver(final Problem problem,
			final SolutionHandler solutionHandler) {
		this.problem = problem;
		this.chronometer = new Chronometer();
		this.solutionHandler = solutionHandler;

	}

	protected void setSolver(Solver solver) {
		this.solver = solver;
	}

	public synchronized void run() {
		chronometer.startChrono();
		try {
			float maxTries = 1;

			do {
				logger.info(solver.toString());
				float localTime = -chronometer.getCurrentChrono();
				int assign = -solver.getNbAssignments();

				if (launch((int) Math.floor(maxTries))) {
					logger.info("Result from " + solver + " !");
					if (solver.getNbSolutions()>0) {
						solutionHandler.setSolution(solver.getSolution()) ;
					} else {
						solutionHandler.setSolution(null);
					}
					chronometer.validateChrono();
					return;
				}


				localTime += chronometer.getCurrentChrono();
				assign += solver.getNbAssignments();
				logger
						.info("Took "
								+ localTime
								+ " s ("
								+ (solver.getMaxBacktracks()
										* (int) Math.floor(maxTries) / localTime)
								+ " flips per second), " + assign
								+ " assignments made");

				maxTries *= 1.5;

			} while (true);
		} catch (IOException e) {

		}

	}

	protected abstract boolean launch(final int factor) throws IOException;

	public String getXMLConfig() {
		return solver.getXMLConfig();
	}

	protected Problem getProblem() {
		return problem;
	}
	
	protected SolutionHandler getSolutionHandler() {
		return solutionHandler;
	}
	
	public float getUserTime() {
		return chronometer.getCurrentChrono();
	}
}
