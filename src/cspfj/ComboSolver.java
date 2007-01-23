package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.constraint.Constraint;
import cspfj.exception.MaxBacktracksExceededException;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

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

		// final int localBT = (int) (-500 + 8.25 * problem.getMaxDomainSize()
		// * problem.getNbVariables() + 0.24 * Math.pow(problem
		// .getMaxDomainSize()
		// * problem.getNbVariables(), 2));

		final int localBT = Math.max((int) (-50000 + 10000 * Math.log(problem
				.getMaxDomainSize()
				* problem.getNbVariables())), 5000);

		int maxBT = 100 * localBT * problem.getNbVariables()
				/ (problem.getNbConstraints() * problem.getMaxDomainSize());

		float maxTries = 1;

		// boolean alt = false;

		do {
			logger.info("MC with " + (int) Math.floor(maxTries) + " x "
					+ localBT + " flips");
			float localTime = -chronometer.getCurrentChrono();
			for (int i = (int) Math.floor(maxTries); --i >= 0;) {
				if (minConflicts(localBT)) {
					return true;
				}

				for (Variable v : problem.getVariables()) {
					v.resetAssign();
				}

				problem.restoreAll(1);
			}
			localTime += chronometer.getCurrentChrono();
			logger.info("Took " + localTime + " s ("
					+ (localBT * (int) Math.floor(maxTries) / localTime)
					+ " flips per second)");

			logger.info("MAC with " + maxBT + " bt");
			float macTime = -chronometer.getCurrentChrono();
			if (mac(maxBT)) {
				return getNbSolutions() > 0;
			}

			problem.restoreAll(1);
			macTime += chronometer.getCurrentChrono();

			logger.info("Took " + macTime + " s (" + (maxBT / macTime)
					+ " bt per second)");

			maxTries *= 1.5 ;
			maxBT *= 1.5 * localTime/macTime ;

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
		logger.info("Max constraint weight : " + problem.getMaxWeight());

		macSolver.addNoGoods();

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

		// for (Constraint c: problem.getConstraints()) {
		// c.setWeight(c.getWeight() / problem.getNbConstraints());
		// }
		logger.info("Max constraint weight : " + problem.getMaxWeight());

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
