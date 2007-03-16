package cspfj;

import java.util.Map;
import java.util.logging.Logger;

import cspfj.filter.AC3_P;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Combo2 extends AbstractSolver {

	final private Heuristic heuristic;

	final private boolean reverse;

	final private static Logger logger = Logger.getLogger("cspfj.ComboSolver2");

	public Combo2(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic, boolean reverse) {
		super(prob, resultHandler);

		this.reverse = reverse;

		this.heuristic = heuristic;
	}

	public String getXMLConfig() {
		final StringBuffer sb = new StringBuffer(150);

		// sb.append("\t\t\t<macSolver>\n").append(macSolver.getXMLConfig())
		// .append("\t\t\t</macSolver>\n") ;

		// sb.append("\t\t\t<mcSolver>\n").append(mCSolver.getXMLConfig()).append(
		// "\t\t\t</mcSolver>\n") ;
		//		
		sb.append("\t\t\t<space>").append(useSpace()).append("</space>\n");

		return sb.toString();
	}

	public synchronized boolean runSolver() {
		System.gc();
		chronometer.startChrono();
		final Filter preprocessor;
		switch (useSpace()) {
		case BRANCH:
			preprocessor = new SAC(problem, new AC3_P(problem), true);
			break;

		case CLASSIC:
			preprocessor = new SAC(problem, new AC3_P(problem), false);
			break;

		default:
			preprocessor = new AC3_P(problem);
		}

		if (!preprocessor.reduceAll(0)) {
			chronometer.validateChrono();
			return false;
		}
		heuristic.compute();

		final SolutionHandler solutionHandler = new SolutionHandler();

		RunMGAC macSolver = null;
		// try {
		macSolver = new RunMGAC(problem, getResultHandler(), heuristic,
				reverse, solutionHandler);
		// } catch (CloneNotSupportedException e) {
		// // TODO Auto-generated catch block
		// e.printStackTrace();
		// }
		statistics("prepro-cpu", chronometer.getCurrentChrono());
		RunWMC mCSolver = null;
		try {
			mCSolver = new RunWMC(problem.clone(), getResultHandler(),
					solutionHandler);
		} catch (CloneNotSupportedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// mCSolver.setPriority(Thread.MIN_PRIORITY);
		// macSolver.setPriority(Thread.MAX_PRIORITY);
		macSolver.start();
		mCSolver.start();

		Map<Variable, Integer> solution = null;
		try {
			solution = solutionHandler.getSolution();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		chronometer.validateChrono();

		statistics("mac-cpu", macSolver.getUserTime());
		statistics("mc-cpu", mCSolver.getUserTime());

		if (solution == null) {
			return false;
		}
		this.setSolution(solution);
		return true;

	}

}
