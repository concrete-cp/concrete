package cspfj;

import java.util.Map;

import cspfj.filter.AC3;
import cspfj.filter.Filter;
import cspfj.filter.SAC;
import cspfj.heuristic.Heuristic;
import cspfj.problem.Problem;
import cspfj.problem.Variable;

public class Combo2 extends AbstractSolver {

	final private Heuristic heuristic;

	// final private static Logger logger =
	// Logger.getLogger("cspfj.ComboSolver2");

	public Combo2(Problem prob, ResultHandler resultHandler,
			Heuristic heuristic) {
		super(prob, resultHandler);

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

	public boolean runSolver() {
		System.gc();
		chronometer.startChrono();
		
		if (!preprocess(new AC3(problem))) {
			chronometer.validateChrono();
			return false;
		}
		heuristic.compute();

		final SolutionHandler solutionHandler = new SolutionHandler();

		RunMGAC macSolver = null;
		try {
			macSolver = new RunMGAC(problem, getResultHandler(),
					heuristic, solutionHandler);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		statistics("prepro-cpu", chronometer.getCurrentChrono());
		RunWMC mCSolver = null;
		try {
			mCSolver = new RunWMC(problem.clone(), getResultHandler(), solutionHandler);
		} catch (Exception e) {
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

		macSolver.end();
		mCSolver.end();

		if (solution == null) {
			return false;
		}
		this.setSolution(solution);
		return true;

	}

}
