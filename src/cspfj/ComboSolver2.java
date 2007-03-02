package cspfj;

import java.io.IOException;
import java.util.logging.Logger;

import cspfj.filter.AC3_P;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;

public class ComboSolver2 extends AbstractSolver {

	final private Heuristic heuristic;

	final private boolean reverse;

	final private static Logger logger = Logger.getLogger("cspfj.ComboSolver2");

	public ComboSolver2(Problem prob, ResultHandler resultHandler,
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

//		RunMACSolver macSolver = null;
//		try {
//			macSolver = new RunMACSolver(problem.clone(), getResultHandler(),
//					heuristic, reverse, chronometer);
//		} catch (CloneNotSupportedException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}

		RunMCSolver mCSolver=null;
//		try {
			mCSolver = new RunMCSolver(problem,
					getResultHandler(), reverse, chronometer);
//		} catch (CloneNotSupportedException e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}

//		macSolver.start();
		mCSolver.start();

		final boolean result = mCSolver.getResult();
		chronometer.validateChrono();
		this.setSolution(mCSolver.getSolution());

		return result;
	}

}
