package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.exception.MaxBacktracksExceededException;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Combo extends AbstractSolver {

	private final AbstractLocalSolver mCSolver;

	private final MGAC macSolver;

	private final Heuristic heuristic;

	private final static Logger logger = Logger
			.getLogger("cspfj.solver.ComboSolver");

	// private final NoGoodManager noGoodManager;

	public Combo(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, boolean reverse) {
		super(prob, resultHandler);
		macSolver = new MGAC(prob, resultHandler, heuristic, reverse);
		mCSolver = new WMC(prob, resultHandler, false);
		this.heuristic = heuristic;

	}

	public boolean runSolver() throws IOException {
		System.gc();

		if (!preprocess(macSolver.getFilter())) {
			return false;
		}

		// final int localBT = (int) (-500 + 8.25 * problem.getMaxDomainSize()
		// * problem.getNbVariables() + 0.24 * Math.pow(problem
		// .getMaxDomainSize()
		// * problem.getNbVariables(), 2));

		heuristic.compute();

		final int localBT = 2000;

		int maxBT = localBT / 10;

		float maxTries = 1;
		// boolean alt = false;

		// final Map<Integer, Integer> weights = new HashMap<Integer,
		// Integer>();

		do {
			float localTime = -chronometer.getCurrentChrono();
			for (int i = (int) Math.floor(maxTries); --i >= 0;) {
				logger.info(localBT + " flips");

				int assign = -this.getNbAssignments();

				if (minConflicts(localBT)) {
					return true;
				}

				for (Variable v : problem.getVariables()) {
					v.resetAssign();
				}

				problem.restoreAll(1);

				assign += getNbAssignments();

			}
			localTime += chronometer.getCurrentChrono();
			logger.info("Took " + localTime + " s");

			logger.info("MAC with " + maxBT + " bt");

			float macTime = -chronometer.getCurrentChrono();
			if (mac(maxBT)) {
				statistics("mac-assign", macSolver.getNbAssignments());
				return getNbSolutions() > 0;
			}

			problem.restoreAll(1);
			macTime += chronometer.getCurrentChrono();

			maxTries *= 1.5;
			maxBT *= 1.5 * localTime / macTime;
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

		//macSolver.addNoGoods();

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
