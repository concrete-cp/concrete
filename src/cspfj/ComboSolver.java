package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
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
		mCSolver = new MinConflictsSolver(prob, resultHandler, reverse);

	}

	public boolean run() throws IOException {
		System.gc();

		final Filter preprocessor;
		switch (useSpace()) {
		case BRANCH:
			preprocessor = new SAC(problem, macSolver.getFilter(), true);
			break;

		case CLASSIC:
			preprocessor = new SAC(problem, macSolver.getFilter(), false);
			break;

		default:
			preprocessor = macSolver.getFilter();
		}

		if (!preprocessor.reduceAll(0)) {
			chronometer.validateChrono();
			return false;
		}

		int maxBT = problem.getNbVariables();
		int localBT = (int) Math.sqrt(problem.getNbVariables()
				* problem.getMaxDomainSize());

		// boolean alt = false;

		do {
			if (mac(maxBT)) {
				return getSolution().size() > 0;
			}

			// if (alt) {
			// for (Constraint c : problem.getConstraints()) {
			// c.setWeight(1);
			// }
			// }

			if (minConflicts(localBT)) {
				return true;
			}

			problem.restoreAll(1);
			
			maxBT *= 1.5;
			localBT *= 1.5;

		} while (true);

	}

	private final boolean mac(final int maxBT) {
		macSolver.setMaxBacktracks(maxBT);

		try {
			if (macSolver.mac(0, null)) {
				// logger.info(macSolver.getSolution().toString());
				setSolution(macSolver.getSolution());
			}
			chronometer.validateChrono();
			return true;
		} catch (MaxBacktracksExceededException e) {
			// Continue
		} catch (OutOfMemoryError e) {
			chronometer.validateChrono();
			throw e;
		}

		final int ng = macSolver.addNoGoods();

		logger.info(ng + " noGoods");

		return false;
	}

	private final boolean minConflicts(final int maxLM) throws IOException {
		mCSolver.setMaxBacktracks(maxLM);

		try {

			mCSolver.minConflicts();

			setSolution(mCSolver.getSolution());
			chronometer.validateChrono();
			return true;
		} catch (MaxBacktracksExceededException e) {

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
		return macSolver.getNbAssignments() + mCSolver.getNbAssignments();
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer(150);

		sb.append("\t\t\t<macSolver>\n").append(macSolver.getXMLConfig())
				.append("\t\t\t</macSolver>\n\t\t\t<mcSolver>\n").append(
						mCSolver.getXMLConfig()).append(
						"\t\t\t</mcSolver>\n\t\t\t<space>").append(useSpace())
				.append("</space>\n");

		return sb.toString();
	}

}
